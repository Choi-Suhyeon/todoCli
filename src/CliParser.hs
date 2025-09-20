module CliParser (module CliParser.Options, parseOpts) where

import Data.HashSet qualified as S
import Data.Text    qualified as T

import Options.Applicative
    ( ParserInfo      , Parser     , ReadM
    , strArgument     , command    , option     , help
    , hidden          , short      , long       , auto
    , maybeReader     , metavar    , value      , info
    , parserFailure   , header     , fullDesc   , progDesc
    , renderFailure   , helper     , infoOption , hsubparser
    , simpleVersioner , execParser  
    )
import Options.Applicative.Builder (ParseError(..), prefs, columns)
import Data.Time.Format.ISO8601    (iso8601ParseM, parseFormatExtension, calendarFormat)
import Control.Applicative         ((<|>), (<**>), optional)
import Data.Time.LocalTime         (LocalTime(..), TimeOfDay(..))
import Data.Time.Calendar          (Day(..))
import Data.Version                (showVersion)
import Data.HashSet                (HashSet)
import Data.Text                   (Text)
import Text.Regex.TDFA             ((=~))
import Control.Monad               (mfilter)

import Paths_todoCli (version)

import CliParser.Options

parseOpts :: IO Options
parseOpts = execParser opts

opts :: ParserInfo Options
opts = info (pOptions <**> helper <**> longHelpOpt <**> versionOpt)
    $  fullDesc
    <> progDesc "Organize and track tasks from the command line with commands to add, list, edit, mark, rename, and delete."
    <> header (progName ++ " - A simple command-line task manager")

progName :: String
progName = "todoCli"

versionOpt :: Parser (a -> a)
versionOpt = simpleVersioner $ showVersion version

longHelpOpt :: Parser (a -> a)
longHelpOpt = infoOption (helpAndExtra opts)
    $  hidden
    <> long  "long-help"
    <> help  "Show extended help"
  where
    helpAndExtra :: ParserInfo a -> String
    helpAndExtra pInfo = helpText <> "\n\n" <> extendedText
      where
        failure  = parserFailure (prefs $ columns 80) pInfo (ShowHelpText Nothing) mempty
        helpText = fst $ renderFailure failure progName

    extendedText :: String
    extendedText = unlines
        [ "Overview:"
        , "  A minimal CLI to add, list, edit, mark, and delete tasks"
        , "  Commands may have their own flags, but common input rules are shared"
        , ""
        , "Tags:"
        , "  - Provide tags as a space-separated string (e.g., \"work urgent home\")"
        , "  - Letters A–Z only; case-insensitive (stored uppercased); no digits or symbols"
        , "  - Up to 10 tags; duplicates are ignored; order does not matter"
        , ""
        , "Dates & time:"
        , "  - DEADLINE accepts either a date or a full ISO-8601 local datetime"
        , "    • Date: YYYY-MM-DD  (time defaults to 23:59:59)"
        , "    • Datetime: YYYY-MM-DDTHH:MM:SS"
        , "  - Timezones/offsets are not accepted; values are treated as local time"
        , ""
        , "Status values:"
        , "  - list:    done | undone | due | overdue"
        , "  - delete:  done | overdue"
        , "  (Each command only accepts the statuses listed for that command)"
        , ""
        , "Structure (grammar):"
        , "  " <> progName <> " add    (-n|--name NAME) (-D|--deadline DEADLINE) [-d|--desc DESCRIPTION] [-t|--tags TAGS]"
        , "  " <> progName <> " list   [-t|--tags TAGS] [-s|--status STATUS]"
        , "  " <> progName <> " edit   TARGET_NAME [-n|--name NEW_NAME] [-d|--desc NEW_DESCRIPTION] [-t|--tags NEW_TAGS] [-D|--deadline NEW_DEADLINE]"
        , "  " <> progName <> " mark   (done|undone) NAME"
        , "  " <> progName <> " delete (all | by [-n|--name NAME] [-t|--tags TAGS] [-s|--status STATUS])"
        , ""
        , "Notes:"
        , "  - TAGS is one argument; put all tags in the same quoted string if needed"
        , "  - NAME/DESCRIPTION are free text; quote when they contain spaces"
        , "  - For command-specific flags and details, run: <command> --help"
        , ""
        , "Behavior:"
        , "  - list and delete apply all provided filters with AND (no OR)"
        , "  - Tag filters are also ANDed: a task matches only if its tags include all provided tags"
        , "  - Name matching (edit/mark/delete): substring match on TARGET_NAME/NAME;"
        , "    if it doesn't resolve to exactly one task, the command errors"
        , ""
        , "Examples:"
        , "  $ " <> progName <> " add -n \"Pay bills\" -d \"Electricity and water\" -D 2025-09-01 -t \"finance urgent\""
        , "  $ " <> progName <> " list -s due -t \"finance\""
        , "  $ " <> progName <> " edit \"Pay bills\" --deadline 2025-09-02T10:00 --tags \"finance\""
        , "  $ " <> progName <> " mark done \"Pay bills\""
        , "  $ " <> progName <> " delete by -s overdue -t \"finance\""
        , "  $ " <> progName <> " delete all"
        ]


pOptions :: Parser Options
pOptions = Options <$> pCommand

pCommand :: Parser Command
pCommand = hsubparser 
    $  command "add"    (info (Add    <$> pAddCommand)    $ progDesc "Add a new task with name, description, deadline, and optional tags")
    <> command "list"   (info (List   <$> pListCommand)   $ progDesc "List tasks filtered by status or tags")
    <> command "edit"   (info (Edit   <$> pEditCommand)   $ progDesc "Edit the properties of an existing task")
    <> command "mark"   (info (Mark   <$> pMarkCommand)   $ progDesc "Mark a task as done or undone")
    <> command "delete" (info (Delete <$> pDeleteCommand) $ progDesc "Delete tasks by name, status, or tags")

pAddCommand :: Parser AddCommand
pAddCommand = AddCommand <$> pName <*> pDeadline <*> pDesc <*> pTags
  where
    pName :: Parser Text
    pName = option nameReader
        $  short   'n'
        <> long    "name"
        <> metavar "NAME"
        <> help    "Task name"

    pDeadline :: Parser LocalTime
    pDeadline = option datetimeReader
        $  short   'D'
        <> long    "deadline"
        <> metavar "DEADLINE"
        <> help    "Deadline as YYYY-MM-DD or ISO 8601 datetime; date-only defaults to 23:59:59"

    pDesc :: Parser Text
    pDesc = option descReader
        $  short   'd'
        <> long    "desc"
        <> metavar "DESCRIPTION"
        <> help    "Task description"
        <> value   T.empty

    pTags :: Parser (HashSet Text)
    pTags = option textSetReader
        $  short   't'
        <> long    "tags"
        <> metavar "TAGS"
        <> help    "Space-separated tags (max 10)"
        <> value   S.empty

pListCommand :: Parser ListCommand
pListCommand = ListCommand <$> pTags <*> pStatus
  where
    pTags :: Parser (HashSet Text)
    pTags = option textSetReader
        $  short   't'
        <> long    "tags"
        <> metavar "TAGS"
        <> help    "Filter by tags (space-separated)"
        <> value   S.empty

    pStatus :: Parser (Maybe ListStatus)
    pStatus = optional $ option auto
        $  short   's'
        <> long    "status"
        <> metavar "STATUS"
        <> help    "Filter by task status (done, undone, due, or overdue)"
    
pEditCommand :: Parser EditCommand
pEditCommand = EditCommand <$> pTgtName <*> pName <*> pDesc <*> pTags <*> pDeadline
  where
    pTgtName :: Parser Text
    pTgtName = strArgument $ metavar "TARGET_NAME"

    pName :: Parser (Maybe Text)
    pName = optional $ option nameReader
        $  short   'n'
        <> long    "name"
        <> metavar "NEW_NAME"
        <> help    "New task name"

    pDesc :: Parser (Maybe Text)
    pDesc = optional $ option descReader
        $  short   'd'
        <> long    "desc"
        <> metavar "NEW_DESCRIPTION"
        <> help    "New description"

    pTags :: Parser (Maybe (HashSet Text))
    pTags = optional $ option textSetReader
        $  short   't'
        <> long    "tags"
        <> metavar "NEW_TAGS"
        <> help    "New tags (space-separated)"

    pDeadline :: Parser (Maybe LocalTime)
    pDeadline = optional $ option datetimeReader
        $  short   'D'
        <> long    "deadline"
        <> metavar "NEW_DEADLINE"
        <> help    "New deadline as YYYY-MM-DD or ISO 8601 datetime; date-only defaults to 23:59:59"

pMarkCommand :: Parser MarkCommand
pMarkCommand = hsubparser
    $  command "done"   (info (MrkDone   <$> pName) (progDesc "Mark a task as done"))
    <> command "undone" (info (MrkUndone <$> pName) (progDesc "Mark a task as undone"))
    <> metavar "COMMAND"
  where
    pName :: Parser Text
    pName = strArgument $ metavar "NAME"

pDeleteCommand :: Parser DeleteCommand
pDeleteCommand = hsubparser    
    $  command "all" (info (pure DelAll) (progDesc "Delete all tasks"))
    <> command "by"  (info pDelBy $ progDesc "Delete tasks matching filters")
    <> metavar "COMMAND"
  where
    pDelBy :: Parser DeleteCommand
    pDelBy = DelBy <$> pByName <*> pByTags <*> pByStatus
    
    pByName :: Parser (Maybe Text)
    pByName = optional $ option nameReader
        $  short   'n'
        <> long    "name"
        <> metavar "NAME"
        <> help    "Filter by task name"

    pByTags :: Parser (Maybe (HashSet Text))
    pByTags = optional $ option textSetReader
        $  short   't'
        <> long    "tags"
        <> metavar "TAGS"
        <> help    "Filter by tags (space-separated)"

    pByStatus :: Parser (Maybe DeleteStatus)
    pByStatus = optional $ option auto
        $  short   's'
        <> long    "status"
        <> metavar "STATUS"
        <> help    "Filter by task status (done or overdue)"

nameReader :: ReadM Text
nameReader = maybeReader \s -> if
    | length s <= 30 -> Just $ T.pack s
    | otherwise      -> Nothing

descReader :: ReadM Text
descReader = maybeReader \s -> if
    | length s <= 60 -> Just $ T.pack s
    | otherwise      -> Nothing

datetimeReader :: ReadM LocalTime
datetimeReader = maybeReader 
    $   (<|>) 
    <$> fmap fromDayToLocalTime . parseFormatExtension calendarFormat 
    <*> iso8601ParseM
  where
    fromDayToLocalTime :: Day -> LocalTime
    fromDayToLocalTime = flip LocalTime $ TimeOfDay 23 59 59

textSetReader :: ReadM (HashSet Text)
textSetReader = maybeReader 
    $ checkSet'sLenValid 
    . fmap (S.filter (not . T.null) . S.fromList . T.splitOn " ") 
    . checkComponentValid 
    . pure 
    . T.toUpper 
    . T.pack
  where
    checkComponentValid :: Maybe Text -> Maybe Text
    checkComponentValid = mfilter (=~ ("^ *[A-Z]{1,10}( +[A-Z]{1,10})* *$" :: Text))

    checkSet'sLenValid :: Maybe (HashSet Text) -> Maybe (HashSet Text)
    checkSet'sLenValid = mfilter $ liftA2 (&&) (> 0) (<= 4) . length

