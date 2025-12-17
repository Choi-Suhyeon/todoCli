{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Exception (ErrorCall, evaluate, try)
import Control.Monad ((>=>))
import Control.Monad.Except (ExceptT (..), MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State.Strict (MonadState, StateT, runStateT)
import Control.Monad.Writer.Strict (MonadWriter, WriterT, censor, runWriterT)
import Data.Bool (bool)
import Data.Either (fromRight)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Maybe (catMaybes, maybeToList)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone, localTimeToUTC)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)
import Witch

import Data.HashSet qualified as HS
import Data.Text.IO qualified as TIO

import CliParser
import Common
import Domain
import Domain.Serialization
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

instance Show AppError where
    show (DomainE x) = "[E:Logic] " <> show x
    show (EffectE x) = "[E:System] " <> show x
    show (ParserE x) = "[E:Parser] " <> show x

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
            $ ExceptT (pure intermediate) >>= writeData . serialize . UsingCereal . snd

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
                    & liftEitherInto @AppError

            pure reg

main' :: Options -> App ()
main' Options{optCommand, verbose} = censor (bool (const mempty) id verbose) $ runCommand optCommand

runCommand :: Command -> App ()
runCommand (Add x) = runAddCommand x
runCommand (List x) = runListCommand x
runCommand (Edit x) = runEditCommand x
runCommand (Mark x) = runMarkCommand x
runCommand (Delete x) = runDeleteCommand x

runAddCommand :: AddCommand -> App ()
runAddCommand AddCommand{name, deadline, memo, tags} = do
    optionDeadlineToEntryDeadline deadline >>= \utcDeadline -> addTask EntryCreation{name, memo, tags, deadline = utcDeadline}

runListCommand :: ListCommand -> App ()
runListCommand ListCommand{tags, status} = do
    tz <- asks (.tz)
    tasks' <- getTasksWithAllTags tags
    tasks'' <- case status of
        Nothing -> getAllTasks
        Just s -> case s of
            LstDone -> getDoneTasks
            LstUndone -> getUndoneTasks
            LstDue -> getDueTasks
            LstOverdue -> getOverdueTasks

    snapshots <-
        HS.intersection tasks' tasks''
            & getTaskDetails
            & fmap sortTaskDetails

    liftIO . putStrLn $ renderTable (initTaskDetailRenderConfig tz) snapshots
    pure ()

runEditCommand :: EditCommand -> App ()
runEditCommand EditCommand{tgtName, name, deadline, memo, tags} = do
    target <- getUniqueTarget tgtName
    utcDeadline <- traverse optionDeadlineToEntryDeadline deadline

    let
        entryPatch =
            EntryPatch
                { name
                , memo = (<$> memo) \case
                    Remove -> ""
                    Memo m -> m
                , status = Nothing
                , deadline = utcDeadline
                , tags = case tags of
                    Nothing -> Nothing
                    Just Clear -> Just mempty
                    Just (Substitute s) -> Just s
                }

    editTask entryPatch target

runMarkCommand :: MarkCommand -> App ()
runMarkCommand (MrkDone tgtName) = getUniqueTarget tgtName >>= markTask EDone
runMarkCommand (MrkUndone tgtName) = getUniqueTarget tgtName >>= markTask EUndone

runDeleteCommand :: DeleteCommand -> App ()
runDeleteCommand DelAll = getAllTasks >>= deleteTasks
runDeleteCommand DelBy{byName, byTags, byStatus} = do
    tasks' <- traverse getTasksByNameRegex byName
    tasks'' <- traverse getTasksWithAllTags byTags
    tasks''' <- (`traverse` byStatus) \case
        DelDone -> getDoneTasks
        DelOverdue -> getOverdueTasks

    tasks <-
        [tasks', tasks'', tasks''']
            & catMaybes
            & foldr1 HS.intersection
            & evaluate
            & try @ErrorCall
            & liftIO
            >>= \case
                Left _ -> throwError $ ParserE NoOptionsProvided
                Right [] -> throwError $ ParserE DeletionTargetNotFound
                Right xs -> pure xs

    deleteTasks tasks

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
