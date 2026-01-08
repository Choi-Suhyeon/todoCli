{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Arrow ((>>>))
import Control.Exception (ErrorCall, evaluate, try)
import Data.HashSet (HashSet)
import Data.List (foldl1')
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone, localTimeToUTC)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)

import Data.HashSet qualified as HS
import Data.Text.IO qualified as TIO

import CliParser
import Common
import Common.Prelude
import Domain
import Domain.Core
    ( checkIdInvariant
    , checkStatusIndexInvariant
    , checkTagIndexInvariant
    , rebuildIds
    )
import Effect
import View

newtype App a = App
    { unApp :: ReaderT Env (StateT TodoRegistry (ExceptT AppError (WriterT Log IO))) a
    }
    deriving newtype
        ( Applicative
        , Functor
        , Monad
        , MonadError AppError
        , MonadIO
        , MonadReader Env
        , MonadState TodoRegistry
        , MonadWriter Log
        )

runApp
    :: Env -> TodoRegistry -> App a -> IO (Either AppError (a, TodoRegistry), Log)
runApp env reg =
    runWriterT . runExceptT . flip runStateT reg . flip runReaderT env . (.unApp)

data AppError
    = DomainE DomainError
    | EffectE EffectError
    | ParserE ParserError
    | StorageE StorageError

data ParserError
    = MultipleTargetsError
    | TargetNotFoundError
    | DeletionTargetNotFound
    | NoOptionsProvided

instance Show ParserError where
    show MultipleTargetsError = "The specified target name refers to multiple tasks"
    show TargetNotFoundError = "The specified target name does not refer to any task"
    show DeletionTargetNotFound = "No target matches the deletion criteria"
    show NoOptionsProvided = "At least one option is required"

data StorageError = SerializationE String
    deriving (Show)

instance From SerializationError StorageError where
    from (DeserializationFailed e) = SerializationE e

instance From StorageError AppError where
    from = StorageE

instance Show AppError where
    show (DomainE x) = "[E:Logic] " <> show x
    show (EffectE x) = "[E:System] " <> show x
    show (ParserE x) = "[E:Parser] " <> show x
    show (StorageE x) = "[E:Storage] " <> show x

instance From DomainError AppError where
    from = DomainE

instance From EffectError AppError where
    from = EffectE

main :: IO ()
main = do
    opts <- parseOpts
    env <- initEnv
    reg <- loadRegistry
    (intermediate, logs) <- main' opts & runApp env reg
    result <-
        runExceptT
            $ ExceptT (pure intermediate)
            >>= writeData
            . serialize
            . UsingCereal
            . snd

    let
        printLogsEndedWith :: Maybe Text -> IO ()
        printLogsEndedWith = TIO.hPutStr stderr . foldMap id . (logs <>) . into . maybeToList

    case result of
        Right () -> printLogsEndedWith Nothing *> exitSuccess
        Left e -> printLogsEndedWith (show e & into & (<> "\n") & Just) *> exitFailure
  where
    initEnv :: IO Env
    initEnv = do
        now <- getCurrentTime
        tz <- getCurrentTimeZone

        pure Env{now, tz}

    loadRegistry :: (MonadIO m) => m TodoRegistry
    loadRegistry =
        fromRight initTodoRegistry <$> runExceptT do
            raw <- readData
            UsingCereal reg <-
                raw
                    & deserialize @(UsingCereal TodoRegistry)
                    & first (into @StorageError)
                    & liftEitherInto @AppError

            let
                adjust =
                    bool rebuildIds pure
                        $ and
                        $ map
                            ($ reg)
                            [ checkIdInvariant
                            , checkTagIndexInvariant
                            , checkStatusIndexInvariant
                            ]

            liftEitherInto $ adjust reg

main' :: Options -> App ()
main' Options{optCommand, verbose} = censor (bool (const mempty) id verbose) $ runCommand optCommand

runCommand :: Command -> App ()
runCommand (Add x) = runAddCommand x
runCommand (List x) = runListCommand x
runCommand (Edit x) = runEditCommand x
runCommand (Mark x) = runMarkCommand x
runCommand (Delete x) = runDeleteCommand x

runAddCommand :: AddCommand -> App ()
runAddCommand AddCommand{name, deadline, memo, tags, importance} = do
    optionDeadlineToEntryDeadline deadline >>= \utcDeadline -> addTask EntryCreation{name, memo, tags, importance, deadline = utcDeadline}

runListCommand :: ListCommand -> App ()
runListCommand ListCommand{tags, status, importance} = do
    tz <- asks (.tz)
    allTasks <- getAllTasks
    tasks1 <- traverse getTasksWithAllTags tags
    tasks2 <- traverse getTasksWithinImportanceRange importance
    tasks3 <- (`traverse` status) \case
        LstDone -> getDoneTasks
        LstUndone -> getUndoneTasks
        LstDue -> getDueTasks
        LstOverdue -> getOverdueTasks

    snapshots <-
        [tasks1, tasks2, tasks3]
            & catMaybes
            & foldl' HS.intersection allTasks
            & into
            & getTaskDetails
            & fmap sortTaskDetails

    liftIO . putStrLn $ renderTable (initTaskDetailRenderConfig tz) snapshots
    pure ()

runEditCommand :: EditCommand -> App ()
runEditCommand EditCommand{tgtName, name, deadline, memo, tags, importance} = do
    target <- getUniqueTarget tgtName
    utcDeadline <- traverse optionDeadlineToEntryDeadline deadline

    flip
        editTask
        target
        EntryPatch
            { name
            , importance
            , status = Nothing
            , deadline = utcDeadline
            , memo = processMemo memo
            , tags = processTags tags
            }
  where
    processMemo :: Maybe EditMemo -> Maybe Text
    processMemo (Just Remove) = Just ""
    processMemo (Just (Memo m)) = Just m
    processMemo Nothing = Nothing

    processTags :: Maybe EditTags -> Maybe (HashSet Text)
    processTags (Just Clear) = Just mempty
    processTags (Just (Substitute s)) = Just s
    processTags Nothing = Nothing

runMarkCommand :: MarkCommand -> App ()
runMarkCommand (MrkDone tgtName) = getUniqueTarget tgtName >>= markTask EDone
runMarkCommand (MrkUndone tgtName) = getUniqueTarget tgtName >>= markTask EUndone

runDeleteCommand :: DeleteCommand -> App ()
runDeleteCommand DelAll = getAllTasks >>= deleteTasks
runDeleteCommand DelBy{byName, byTags, byStatus, byImportance} = do
    tasks1 <- traverse getTasksByNameRegex byName
    tasks2 <- traverse getTasksWithAllTags byTags
    tasks3 <- traverse getTasksWithinImportanceRange byImportance
    tasks4 <- (`traverse` byStatus) \case
        DelDone -> getDoneTasks
        DelOverdue -> getOverdueTasks

    tasks <-
        [tasks1, tasks2, tasks3, tasks4]
            & catMaybes
            & foldl1' HS.intersection
            & (evaluate >>> try @ErrorCall >>> liftIO >=> handleResult)

    deleteTasks tasks
  where
    handleResult :: Either ErrorCall (HashSet TaskId) -> App (HashSet TaskId)
    handleResult (Left _) = throwError $ ParserE NoOptionsProvided
    handleResult (Right []) = throwError $ ParserE DeletionTargetNotFound
    handleResult (Right xs) = pure xs

getUniqueTarget :: (MonadError AppError m, MonadRegistry m) => Text -> m TaskId
getUniqueTarget = getTasksByNameRegex >=> getFromSingleton

getFromSingleton :: (Foldable f, MonadError AppError m) => f a -> m a
getFromSingleton = (. toList) \case
    [x] -> pure x
    [] -> throwError $ ParserE TargetNotFoundError
    _ -> throwError $ ParserE MultipleTargetsError

optionDeadlineToEntryDeadline
    :: (MonadEnv m) => OptionDeadline -> m EntryDeadline
optionDeadlineToEntryDeadline Boundless = pure EBoundless
optionDeadlineToEntryDeadline (Bound d) = asks (.tz) >>= pure . EBound . (`localTimeToUTC` d)
