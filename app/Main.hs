{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Exception (ErrorCall, evaluate, try)
import Control.Monad ((>=>))
import Control.Monad.Except
    ( ExceptT (..)
    , MonadError
    , runExceptT
    , throwError
    )
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State.Strict (MonadState, StateT, runStateT)
import Data.Either (fromRight)
import Data.Foldable (Foldable (..), foldr1)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone, localTimeToUTC)
import System.Exit (die, exitSuccess)
import Witch
import Prelude hiding (Foldable (..))

import Data.HashSet qualified as S

import CliParser
import Common
import Domain
import Domain.Serialization
import Effect

newtype App a = App {unApp :: ReaderT Env (StateT TodoRegistry (ExceptT AppError IO)) a}
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
        )

runApp :: Env -> TodoRegistry -> App a -> IO (Either AppError (a, TodoRegistry))
runApp env reg = runExceptT . flip runStateT reg . flip runReaderT env . (^. #unApp)

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

    either (die . show) (const exitSuccess) =<< runExceptT do
        ((), reg') <-
            main' opts
                & runApp env reg
                & ExceptT

        UsingCereal reg'
            & serialize
            & writeData
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
main' Options{optCommand} = runCommand optCommand

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
            & getTaskSnapshots
            & fmap sortTaskSnapshots

    liftIO . putStrLn $ renderTable (initTaskSnapshotRenderConfig tz 0 1) snapshots
    pure ()

runEditCommand :: EditCommand -> App ()
runEditCommand EditCommand{tgtName, name, deadline, desc, tags} = do
    Env{tz} <- ask
    target <- getUniqueTarget tgtName

    editTask
        EntryUpdate{name, desc, tags, deadline = localTimeToUTC tz <$> deadline}
        target

runMarkCommand :: MarkCommand -> App ()
runMarkCommand (MrkDone tgtName) = getUniqueTarget tgtName >>= markTask MDone
runMarkCommand (MrkUndone tgtName) = getUniqueTarget tgtName >>= markTask MUndone

runDeleteCommand :: DeleteCommand -> App ()
runDeleteCommand DelAll = getAllTasks >>= deleteTasks
runDeleteCommand DelBy{byName, byTags, byStatus} = do
    tasks' <- traverse getTasksByNameContaining byName
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
getUniqueTarget = getTasksByNameContaining >=> getFromSingleton

getFromSingleton :: (Foldable f, MonadError AppError m) => f a -> m a
getFromSingleton = (. toList) \case
    [x] -> pure x
    [] -> throwError $ ParserE TargetNotFoundError
    _ -> throwError $ ParserE MultipleTargetsError
