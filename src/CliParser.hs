{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}

module CliParser (module CliParser.Options, parseOpts) where

import Control.Arrow ((>>>))
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.HashSet (HashSet)
import Data.Text (Text)
import Data.Time.Calendar (Day (..))
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..))
import Data.Version (showVersion)
import Options.Applicative

import Data.HashSet qualified as HS
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import CliParser.Options
import Common
import External.ISO8601 (calendarFormat, iso8601ParseM, parseFormatExtension)
import External.Interval (Extended (..), Interval, (<=..<=))
import External.Prelude
import External.Regex
import Paths_todo (version)

parseOpts :: (HasConfig) => IO Options
parseOpts = execParser opts

opts :: (HasConfig) => ParserInfo Options
opts =
    info (pOptions <**> helper <**> longHelpOpt <**> versionOpt)
        $ fullDesc
        <> progDesc "Manage your tasks from the command line"
        <> header (progName <> " - A simple command-line task manager")

progName :: String
progName = "todo"

versionOpt :: Parser (a -> a)
versionOpt = simpleVersioner $ showVersion version

longHelpOpt :: Parser (a -> a)
longHelpOpt =
    infoOption longHelpStr
        $ hidden
        <> long "long-help"
        <> help "Show extended help"
  where
    longHelpStr :: String
    longHelpStr = into $ TE.decodeUtf8 longHelpRaw

    longHelpRaw :: ByteString
    longHelpRaw = $(embedFile "docs/long-help.txt")

pOptions :: (HasConfig) => Parser Options
pOptions = Options <$> pCommand <*> pVerbose

pVerbose :: Parser Bool
pVerbose =
    switch
        $ short 'v'
        <> long "verbose"
        <> help "Show verbose logs during processing"

pCommand :: (HasConfig) => Parser Command
pCommand = hsubparser $ cAdd <> cList <> cEdit <> cMark <> cDelete
  where
    cAdd, cList, cEdit, cMark, cDelete :: Mod CommandFields Command

    cAdd =
        command "add"
            $ info (Add <$> pAddCommand)
            $ progDesc "Add a new task with name, memo, deadline, and tags"

    cList =
        command "list"
            $ info (List <$> pListCommand)
            $ progDesc "List tasks filtered by status or tags"

    cEdit =
        command "edit"
            $ info (Edit <$> pEditCommand)
            $ progDesc "Edit the properties of an existing task"

    cMark =
        command "mark"
            $ info (Mark <$> pMarkCommand)
            $ progDesc "Mark a task as done or undone"

    cDelete =
        command "delete"
            $ info (Delete <$> pDeleteCommand)
            $ progDesc "Delete all tasks or delete selectively by filters"

pAddCommand :: (HasConfig) => Parser AddCommand
pAddCommand = AddCommand <$> pName <*> pDeadline <*> pMemo <*> pTags <*> pImportance
  where
    pName :: Parser Text
    pName =
        option simpleTextReader
            $ short 'n'
            <> long "name"
            <> metavar "NAME"
            <> help "Task name"

    pDeadline :: Parser OptionDeadline
    pDeadline =
        option datetimeReader
            $ short 'd'
            <> long "deadline"
            <> metavar "DEADLINE"
            <> help
                "Deadline (YYYY-MM-DD or ISO 8601 datetime; time defaults to 23:59:59)"
            <> value Boundless

    pMemo :: Parser Text
    pMemo =
        option simpleTextReader
            $ short 'm'
            <> long "memo"
            <> metavar "MEMO"
            <> help "Additional task information"
            <> value T.empty

    pTags :: Parser (HashSet Text)
    pTags =
        option textSetReader
            $ short 't'
            <> long "tags"
            <> metavar "TAGS"
            <> help "Space-separated tags (maximum 10)"
            <> value HS.empty

    pImportance :: Parser Word
    pImportance =
        option importanceReader
            $ short 'i'
            <> long "importance"
            <> metavar "IMP."
            <> help helpMsg
            <> value ?config.importanceDefault
      where
        helpMsg :: String
        helpMsg =
            mconcat
                [ "Importance ranging from 1 to 9 (default: "
                , show ?config.importanceDefault
                , ")"
                ]

pListCommand :: (HasConfig) => Parser ListCommand
pListCommand = ListCommand <$> pTags <*> pStatus <*> pImportance <*> pShouldReverse
  where
    pTags :: Parser (Maybe (HashSet Text))
    pTags =
        optional
            $ option textSetReader
            $ short 't'
            <> long "tags"
            <> metavar "TAGS"
            <> help "Filter by tags (space-separated)"

    pStatus :: Parser (Maybe ListStatus)
    pStatus =
        optional
            $ option auto
            $ short 's'
            <> long "status"
            <> metavar "STATUS"
            <> help "Filter by status (done, undone, due, overdue)"

    pImportance :: Parser (Maybe (Interval Word))
    pImportance =
        optional
            $ option importanceRangeReader
            $ short 'i'
            <> long "importance"
            <> metavar "IMP."
            <> help "Filter by importance range"

    pShouldReverse :: Parser Bool
    pShouldReverse =
        switch
            $ short 'r'
            <> long "reverse"
            <> help "Reverse the order of the list"

pEditCommand :: (HasConfig) => Parser EditCommand
pEditCommand =
    EditCommand
        <$> pTgtName
        <*> pName
        <*> pMemo
        <*> pTags
        <*> pDeadline
        <*> pImportance
  where
    pTgtName :: Parser Text
    pTgtName = strArgument $ metavar "NAME_PATTERN"

    pName :: Parser (Maybe Text)
    pName =
        optional
            $ option simpleTextReader
            $ short 'n'
            <> long "name"
            <> metavar "NAME"
            <> help "New task name"

    pMemo :: Parser (Maybe EditMemo)
    pMemo = optional $ remove <|> memo
      where
        remove =
            flag' Remove
                $ short 'M'
                <> long "clear-memo"
                <> help "Clear the task memo"
        memo =
            option (Memo <$> simpleTextReader)
                $ short 'm'
                <> long "memo"
                <> metavar "MEMO"
                <> help "New additional task information"

    pTags :: Parser (Maybe EditTags)
    pTags = optional $ clear <|> substitute
      where
        clear =
            flag' Clear
                $ short 'T'
                <> long "clear-tag"
                <> help "Clear all task tags"

        substitute =
            option (Substitute <$> textSetReader)
                $ short 't'
                <> long "tags"
                <> metavar "TAGS"
                <> help "New tags (space-separated)"

    pDeadline :: Parser (Maybe OptionDeadline)
    pDeadline = optional $ boundless <|> bound
      where
        boundless =
            flag' Boundless
                $ short 'D'
                <> long "clear-deadline"
                <> help "Clear the task deadline"

        bound =
            option datetimeReader
                $ short 'd'
                <> long "deadline"
                <> metavar "DEADLINE"
                <> help
                    "New deadline (YYYY-MM-DD or ISO 8601 datetime; time defaults to 23:59:59)"

    pImportance :: Parser (Maybe Word)
    pImportance =
        optional
            $ option importanceReader
            $ short 'i'
            <> long "importance"
            <> metavar "IMP."
            <> help "Importance ranging from 1 to 9"

pMarkCommand :: Parser MarkCommand
pMarkCommand =
    hsubparser
        $ command "done" (info (MrkDone <$> pName) (progDesc "Mark a task as done"))
        <> command "undone" (info (MrkUndone <$> pName) (progDesc "Mark a task as undone"))
        <> metavar "COMMAND"
  where
    pName :: Parser Text
    pName = strArgument $ metavar "NAME_PATTERN"

pDeleteCommand :: (HasConfig) => Parser DeleteCommand
pDeleteCommand =
    hsubparser
        $ command "all" (info (pure DelAll) (progDesc "Delete all tasks"))
        <> command "by" (info pDelBy $ progDesc "Delete tasks matching filters")
        <> metavar "COMMAND"
  where
    pDelBy :: Parser DeleteCommand
    pDelBy = DelBy <$> pByName <*> pByTags <*> pByStatus <*> pByImportance

    pByName :: Parser (Maybe Text)
    pByName =
        optional
            $ strOption
            $ short 'n'
            <> long "name"
            <> metavar "NAME_PATTERN"
            <> help "Filter by task name"

    pByTags :: Parser (Maybe (HashSet Text))
    pByTags =
        optional
            $ option textSetReader
            $ short 't'
            <> long "tags"
            <> metavar "TAGS"
            <> help "Filter by tags (space-separated)"

    pByStatus :: Parser (Maybe DeleteStatus)
    pByStatus =
        optional
            $ option auto
            $ short 's'
            <> long "status"
            <> metavar "STATUS"
            <> help "Filter by task status (done, overdue)"

    pByImportance :: Parser (Maybe (Interval Word))
    pByImportance =
        optional
            $ option importanceRangeReader
            $ short 'i'
            <> long "importance"
            <> metavar "IMP."
            <> help "Filter by task importance"

simpleTextReader :: ReadM Text
simpleTextReader =
    eitherReader $ liftA2 (bool $ Left errMsg) Right isValidFormat . T.strip . into
  where
    isValidFormat :: Text -> Bool
    isValidFormat = (=~ ("^[^[:blank:]]+([[:blank:]][^[:blank:]]+)*$" :: Text))

    errMsg :: String
    errMsg = "Text format mismatch: multiple consecutive spaces are not allowed"

datetimeReader :: ReadM OptionDeadline
datetimeReader =
    eitherReader
        $ maybeToEither errMsg
        . asum
        . flip
            map
            [ parseCalendarFormat
            , parseISO8601Format
            ]
        . (&)
  where
    parseCalendarFormat, parseISO8601Format :: String -> Maybe OptionDeadline

    parseCalendarFormat = fmap (Bound . fromDayToLocalTime) . parseFormatExtension calendarFormat
    parseISO8601Format = fmap Bound . iso8601ParseM

    fromDayToLocalTime :: Day -> LocalTime
    fromDayToLocalTime = (`LocalTime` TimeOfDay 23 59 59)

    errMsg :: String
    errMsg =
        "Date format mismatch: only ISO 8601, yyyy-mm-dd, or yyyymmdd formats are allowed"

textSetReader :: ReadM (HashSet Text)
textSetReader =
    eitherReader
        $ liftA2
            (bool $ Left errMsg)
            (Right . uniqueWords)
            isValidFormat
        . T.toUpper
        . T.strip
        . into
  where
    isValidFormat :: Text -> Bool
    isValidFormat = (=~ ("^[^[:space:]]+( +[^[:space:]]+)*$" :: Text))

    uniqueWords :: Text -> HashSet Text
    uniqueWords = HS.filter (not . T.null) . HS.fromList . T.splitOn " "

    errMsg :: String
    errMsg =
        "List format mismatch: elements must be separated by spaces, and only the space character is allowed as whitespace"

importanceReader :: (HasConfig) => ReadM Word
importanceReader = eitherReader parseImportanceSyntax

importanceRangeReader :: (HasConfig) => ReadM (Interval Word)
importanceRangeReader = eitherReader parseRange
  where
    parseRange :: String -> Either String (Interval Word)
    parseRange =
        LS.splitOn ".." >>> \case
            [l, u] ->
                (<=..<=)
                    <$> parseBound NegInf (strip l)
                    <*> parseBound PosInf (strip u)
            _ -> Left "Range format mismatch: must be written in N..M format"
      where
        parseBound :: Extended Word -> String -> Either String (Extended Word)
        parseBound defaultVal "" = Right defaultVal
        parseBound _ s = Finite <$> parseImportanceSyntax s

        strip = L.dropWhile isSpace . L.dropWhileEnd isSpace

parseImportanceSyntax :: (HasConfig) => String -> Either String Word
parseImportanceSyntax =
    bool
        <$> readAlias
        . map toLower
        <*> Right
        . read
        <*> all isDigit
  where
    readAlias :: String -> Either String Word
    readAlias "low" = Right 2
    readAlias "default" = Right $ ?config.importanceDefault
    readAlias "important" = Right 6
    readAlias "critical" = Right 8
    readAlias _ =
        Left
            "Alias mismatch: only low, default, important, and critical are allowed as aliases"
