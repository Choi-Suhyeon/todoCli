module Domain
    ( module Domain.Error
    , module Domain.Log
    , MonadRegistry
    , TodoRegistry
    , TaskId
    , EntryCreate (..)
    , EntryPatch (..)
    , PatchStatus (..)
    , TaskBasic (..)
    , TaskDetail (..)
    , TaskStatusDetail (..)
    , initTodoRegistry
    , getAllTasks
    , getTasksWithAllTags
    , getTasksByNameRegex
    , getDoneTasks
    , getUndoneTasks
    , getOverdueTasks
    , getDueTasks
    , addTask
    , editTask
    , markTask
    , deleteTasks
    , getTaskDetails
    ) where

import Control.Monad (when)
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (MonadState (..), modify')
import Data.Either (fromRight)
import Data.Foldable (for_)
import Data.HashSet (HashSet)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Text (Text)
import Witch

import Data.HashSet qualified as S
import Data.Text qualified as T

import Common
import Domain.Error
import Domain.Log
import Domain.TaskId
import Domain.TodoRegistry
    ( EntryCreate
    , EntryPatch (..)
    , PatchStatus
    , TaskBasic (..)
    , TaskDetail
    , TaskStatusDetail (..)
    , TodoRegistry
    , initTodoRegistry
    )

import Domain.TodoRegistry qualified as TR

type MonadRegistry m = MonadState TodoRegistry m

addTask
    :: (MonadDomainError e m, MonadEnv m, MonadLog m, MonadRegistry m)
    => EntryCreate -> m ()
addTask e = do
    Env{tz, now} <- ask
    newTask <- liftEitherInto $ TR.mkTask tz now e

    modify' $ TR.insertTask newTask
    logMsg $ "task added:\n" <> renderTaskDetail tz (TR.toTaskDetail now newTask)

editTask
    :: (MonadDomainError e m, MonadEnv m, MonadLog m, MonadRegistry m)
    => EntryPatch
    -> TaskId
    -> m ()
editTask e tid = do
    Env{..} <- ask
    reg <- get

    (oldTask, newTask) <- liftEitherInto do
        old <- maybeToEither TaskNotFound $ TR.getTaskById tid reg
        new <- TR.modifyTask tz now e old

        pure (old, new)

    let
        msgForOld = renderTaskDetail tz $ TR.toTaskDetail now oldTask
        msgForNew = renderTaskDetail tz $ TR.toTaskDetail now newTask

    logMsg $ "task updated:\n  (old)\n" <> msgForOld <> "\n  (new)\n" <> msgForNew
    put $ TR.updateTask tid newTask reg

markTask
    :: (MonadEnv m, MonadLog m, MonadRegistry m) => PatchStatus -> TaskId -> m ()
markTask s tid = do
    Env{tz, now} <- ask
    reg <- get

    when (TR.isIdInUse tid reg) do
        let
            task = fromJust $ TR.getTaskById tid reg
            TaskBasic{name} = TR.toTaskBasic task
            entry =
                EntryPatch
                    { name = Nothing
                    , desc = Nothing
                    , tags = Nothing
                    , deadline = Nothing
                    , status = Just s
                    }

        logMsg $ "task marked " <> (show s & into & T.toLower) <> ": '" <> name <> "'"
        put
            $ TR.updateTask tid (fromRight undefined $ TR.modifyTask tz now entry task) reg

deleteTasks
    :: (MonadEnv m, MonadLog m, MonadRegistry m) => HashSet TaskId -> m ()
deleteTasks taskIds = do
    Env{..} <- ask
    reg <- get

    for_ taskIds
        $ liftA2
            when
            isJust
            ( logMsg
                . ("task deleted: " <>)
                . renderTaskSummary
                . (TR.toTaskDetail now)
                . fromJust
            )
            . (`TR.getTaskById` reg)
    put $ S.foldl' (flip TR.deleteTask) reg taskIds

getTaskDetails
    :: (MonadEnv m, MonadRegistry m) => HashSet TaskId -> m [TaskDetail]
getTaskDetails tids = do
    Env{..} <- ask
    reg <- get

    tids
        & S.toList
        & mapMaybe ((TR.toTaskDetail now <$>) . (`TR.getTaskById` reg))
        & pure

getAllTasks :: (MonadRegistry m) => m (HashSet TaskId)
getAllTasks = get >>= pure . TR.getAllTasks

getDoneTasks :: (MonadRegistry m) => m (HashSet TaskId)
getDoneTasks = get >>= pure . TR.getDoneTasks

getUndoneTasks :: (MonadRegistry m) => m (HashSet TaskId)
getUndoneTasks = get >>= pure . TR.getUndoneTasks

getOverdueTasks :: (MonadEnv m, MonadRegistry m) => m (HashSet TaskId)
getOverdueTasks = ask >>= \Env{now} -> get >>= pure . TR.getOverdueTasks now

getDueTasks :: (MonadEnv m, MonadRegistry m) => m (HashSet TaskId)
getDueTasks = ask >>= \Env{now} -> get >>= pure . TR.getDueTasks now

getTasksByNameRegex :: (MonadRegistry m) => Text -> m (HashSet TaskId)
getTasksByNameRegex pattern = get >>= pure . TR.getTasksByNameRegex pattern

getTasksWithAllTags :: (MonadRegistry m) => HashSet Text -> m (HashSet TaskId)
getTasksWithAllTags tags = get >>= pure . TR.getTasksWithAllTags tags

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing = Left a
