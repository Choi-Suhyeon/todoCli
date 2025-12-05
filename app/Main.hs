{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Exception (ErrorCall, evaluate, try)
import Control.Monad ((>=>))
import Control.Monad.Except (ExceptT (..), MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State.Strict (MonadState, StateT, runStateT)
import Control.Monad.Writer.Strict (MonadWriter, WriterT, censor, runWriterT)
import Data.Bool (bool)
import Data.Either (fromRight)
import Data.Foldable (Foldable (..), foldr1)
import Data.Maybe (catMaybes, maybeToList)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone, localTimeToUTC)
import System.Exit (exitFailure, exitSuccess)
import Witch
import Prelude hiding (Foldable (..))

import Data.HashSet qualified as S
import Data.Text.IO qualified as TIO

import CliParser
import Common
import Domain
import Domain.Serialization
import Effect

newtype App a = App
    { unApp :: ReaderT Env (StateT TodoRegistry (ExceptT AppError (WriterT Log IO))) a
    }
    deriving stock
        ( Generic
        )
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
    runWriterT . runExceptT . flip runStateT reg . flip runReaderT env . (^. #unApp)

data AppError
    = DomainE DomainError
    | EffectE EffectError
    | ParserE ParserError
    deriving (Generic)

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
            (ExceptT (pure intermediate) >>= writeData . serialize . UsingCereal . snd)

    let
        printLogsEndedWith :: Maybe Text -> IO ()
        printLogsEndedWith = TIO.putStr . foldMap id . (logs <>) . into . maybeToList

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
runAddCommand AddCommand{name, deadline, desc, tags} =
    ask >>= \Env{tz} -> addTask EntryCreate{name, desc, tags, deadline = localTimeToUTC tz deadline}

runListCommand :: ListCommand -> App ()
runListCommand ListCommand{tags, status} = do
    Env{tz} <- ask
    tasks' <- getTasksWithAllTags tags
    tasks'' <- case status of
        Nothing -> getAllTasks
        Just s -> case s of
            LstDone -> getDoneTasks
            LstUndone -> getUndoneTasks
            LstDue -> getDueTasks
            LstOverdue -> getOverdueTasks

    snapshots <-
        S.intersection tasks' tasks''
            & getTaskDetails
            & fmap sortTaskDetails

    liftIO . putStrLn $ renderTable (initTaskSnapshotRenderConfig tz 0 1) snapshots
    pure ()

runEditCommand :: EditCommand -> App ()
runEditCommand EditCommand{tgtName, name, deadline, desc, tags} = do
    Env{tz} <- ask
    target <- getUniqueTarget tgtName

    editTask
        EntryPatch
            { name
            , desc
            , tags
            , deadline = localTimeToUTC tz <$> deadline
            , status = Nothing
            }
        target

runMarkCommand :: MarkCommand -> App ()
runMarkCommand (MrkDone tgtName) = getUniqueTarget tgtName >>= markTask PDone
runMarkCommand (MrkUndone tgtName) = getUniqueTarget tgtName >>= markTask PUndone

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
            & foldr1 S.intersection
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
