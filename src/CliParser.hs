{-# LANGUAGE TemplateHaskell #-}

module CliParser (module CliParser.Options, parseOpts) where

import Control.Monad (mfilter)
import Data.ByteString (ByteString)
import Data.Bool (bool)
import Data.HashSet (HashSet)
import Data.Text (Text)
import Data.Time.Calendar (Day (..))
import Data.Time.Format.ISO8601
    ( calendarFormat
    , iso8601ParseM
    , parseFormatExtension
    )
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..))
import Data.Version (showVersion)
import Options.Applicative
import Text.Regex.TDFA ((=~))
import Witch

import Data.HashSet qualified as S
import Data.Text qualified as T
import qualified Data.Text.Encoding as TE

import CliParser.Options
import Paths_todo (version)
import Data.FileEmbed (embedFile)

parseOpts :: IO Options
parseOpts = execParser opts

opts :: ParserInfo Options
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

pOptions :: Parser Options
pOptions = Options <$> pCommand <*> pVerbose

pVerbose :: Parser Bool
pVerbose =
    switch
        $ short 'v'
            <> long "verbose"
            <> help "Show verbose logs during processing"

pCommand :: Parser Command
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

pAddCommand :: Parser AddCommand
pAddCommand = AddCommand <$> pName <*> pDeadline <*> pMemo <*> pTags
  where
    pName :: Parser Text
    pName =
        option nameReader
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
        option descReader
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
                <> value S.empty

pListCommand :: Parser ListCommand
pListCommand = ListCommand <$> pTags <*> pStatus
  where
    pTags :: Parser (HashSet Text)
    pTags =
        option textSetReader
            $ short 't'
                <> long "tags"
                <> metavar "TAGS"
                <> help "Filter by tags (space-separated)"
                <> value S.empty

    pStatus :: Parser (Maybe ListStatus)
    pStatus =
        optional
            $ option auto
            $ short 's'
                <> long "status"
                <> metavar "STATUS"
                <> help "Filter by status (done, undone, due, overdue)"

pEditCommand :: Parser EditCommand
pEditCommand = EditCommand <$> pTgtName <*> pName <*> pMemo <*> pTags <*> pDeadline
  where
    pTgtName :: Parser Text
    pTgtName = strArgument $ metavar "NAME_PATTERN"

    pName :: Parser (Maybe Text)
    pName =
        optional
            $ option nameReader
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
            option (Memo <$> descReader)
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

pMarkCommand :: Parser MarkCommand
pMarkCommand =
    hsubparser
        $ command "done" (info (MrkDone <$> pName) (progDesc "Mark a task as done"))
            <> command "undone" (info (MrkUndone <$> pName) (progDesc "Mark a task as undone"))
            <> metavar "COMMAND"
  where
    pName :: Parser Text
    pName = strArgument $ metavar "NAME_PATTERN"

pDeleteCommand :: Parser DeleteCommand
pDeleteCommand =
    hsubparser
        $ command "all" (info (pure DelAll) (progDesc "Delete all tasks"))
            <> command "by" (info pDelBy $ progDesc "Delete tasks matching filters")
            <> metavar "COMMAND"
  where
    pDelBy :: Parser DeleteCommand
    pDelBy = DelBy <$> pByName <*> pByTags <*> pByStatus

    pByName :: Parser (Maybe Text)
    pByName =
        optional
            $ option nameReader
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

nameReader :: ReadM Text
nameReader = maybeReader $ liftA2 (bool Nothing) (Just . T.pack) ((<= 30) . length)

descReader :: ReadM Text
descReader = maybeReader $ liftA2 (bool Nothing) (Just . T.pack) ((<= 60) . length)

datetimeReader :: ReadM OptionDeadline
datetimeReader =
    maybeReader
        $ (<|>)
            <$> fmap (Bound . fromDayToLocalTime) . parseFormatExtension calendarFormat
            <*> fmap Bound . iso8601ParseM
  where
    fromDayToLocalTime :: Day -> LocalTime
    fromDayToLocalTime = (`LocalTime` TimeOfDay 23 59 59)

textSetReader :: ReadM (HashSet Text)
textSetReader =
    maybeReader
        $ mfilter isValidSetLen
            . fmap uniqueWords
            . mfilter isValidFormat
            . pure
            . T.toUpper
            . T.pack
  where
    isValidFormat :: Text -> Bool
    isValidFormat = (=~ ("^ *[A-Z]{1,10}( +[A-Z]{1,10})* *$" :: Text))

    isValidSetLen :: HashSet a -> Bool
    isValidSetLen = liftA2 (&&) (> 0) (<= 4) . length

    uniqueWords :: Text -> HashSet Text
    uniqueWords = S.filter (not . T.null) . S.fromList . T.splitOn " "
