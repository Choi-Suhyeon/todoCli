module Domain
    ( module Domain.Error
    , module Domain.Log
    , MonadRegistry
    , TodoRegistry
    , TaskId
    , EntryCreation (..)
    , EntryPatch (..)
    , EntryDeadline (..)
    , EntryStatus (..)
    , TaskBasic (..)
    , TaskDetail (..)
    , TaskDetailDeadline (..)
    , TaskDetailStatus (..)
    , C.initTodoRegistry
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
    , getTasksWithinImportanceRange
    ) where

import Data.HashSet (HashSet)
import Data.Interval (Interval, member)
import Data.Text (Text)

import Data.HashSet qualified as HS
import Data.Text qualified as T

import Common
import Common.Prelude
import Domain.Core
    ( EntryCreation (..)
    , EntryDeadline (..)
    , EntryPatch (..)
    , EntryStatus (..)
    , TaskBasic (..)
    , TaskId
    , TodoRegistry
    )
import Domain.Error
import Domain.Log
import Domain.TaskDetail

import Domain.Core qualified as C

type MonadRegistry m = MonadState TodoRegistry m

addTask
    :: (MonadDomainError e m, MonadEnv m, MonadLog m, MonadRegistry m)
    => EntryCreation -> m ()
addTask e = do
    reg <- get
    Env{tz, now} <- ask
    newTask <- liftEitherInto $ C.mkTask tz now e
    reg' <- liftEitherInto $ C.insertTask newTask reg

    put reg'
    logMsg $ "task added:\n" <> renderTaskDetail tz (toTaskDetail now newTask)

editTask
    :: (MonadDomainError e m, MonadEnv m, MonadLog m, MonadRegistry m)
    => EntryPatch
    -> TaskId
    -> m ()
editTask e tid = do
    Env{..} <- ask
    reg <- get

    (oldTask, newTask) <- liftEitherInto do
        old <- maybeToEither TaskNotFound $ C.getTaskById tid reg
        new <- C.modifyTask tz now e old

        pure (old, new)

    let
        msgForOld = renderTaskDetail tz $ toTaskDetail now oldTask
        msgForNew = renderTaskDetail tz $ toTaskDetail now newTask

    logMsg $ "task updated:\n  (old)\n" <> msgForOld <> "\n  (new)\n" <> msgForNew
    put $ C.replaceTask tid newTask reg

markTask
    :: (MonadEnv m, MonadLog m, MonadRegistry m) => EntryStatus -> TaskId -> m ()
markTask s tid = do
    Env{tz, now} <- ask
    reg <- get

    when (C.isIdInUse tid reg) do
        let
            task = fromJust $ C.getTaskById tid reg
            TaskBasic{name} = C.toTaskBasic task
            entry =
                EntryPatch
                    { name = Nothing
                    , memo = Nothing
                    , tags = Nothing
                    , deadline = Nothing
                    , importance = Nothing
                    , status = Just s
                    }

        logMsg $ "task marked " <> (show s & into & T.toLower) <> ": '" <> name <> "'"
        put
            $ C.replaceTask tid (fromRight undefined $ C.modifyTask tz now entry task) reg

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
                . (toTaskDetail now)
                . fromJust
            )
        . (`C.getTaskById` reg)
    put $ HS.foldl' (flip C.deleteTask) reg taskIds

getTaskDetails
    :: (MonadEnv m, MonadRegistry m) => HashSet TaskId -> m [TaskDetail]
getTaskDetails tids = do
    Env{..} <- ask
    reg <- get

    tids
        & HS.toList
        & mapMaybe ((toTaskDetail now <$>) . (`C.getTaskById` reg))
        & pure

getAllTasks :: (MonadRegistry m) => m (HashSet TaskId)
getAllTasks = get >>= pure . C.getTasksMatching (const True)

getDoneTasks :: (MonadRegistry m) => m (HashSet TaskId)
getDoneTasks = get >>= pure . C.getDoneTasks

getUndoneTasks :: (MonadRegistry m) => m (HashSet TaskId)
getUndoneTasks = get >>= pure . C.getUndoneTasks

getOverdueTasks :: (MonadEnv m, MonadRegistry m) => m (HashSet TaskId)
getOverdueTasks = ask >>= \Env{now} -> get >>= pure . C.getTasksUndoneAnd (isOverdue now)

getDueTasks :: (MonadEnv m, MonadRegistry m) => m (HashSet TaskId)
getDueTasks = ask >>= \Env{now} -> get >>= pure . C.getTasksUndoneAnd (isDue now)

getTasksByNameRegex :: (MonadRegistry m) => Text -> m (HashSet TaskId)
getTasksByNameRegex pattern = get >>= pure . C.getTasksMatching (matchTest compiled . into @Text . (.name))
  where
    compiled :: Regex
    compiled = makeRegex pattern

getTasksWithAllTags :: (MonadRegistry m) => HashSet Text -> m (HashSet TaskId)
getTasksWithAllTags tags = get >>= pure . C.getTasksWithAllTags tags

getTasksWithinImportanceRange
    :: (MonadRegistry m) => Interval Word -> m (HashSet TaskId)
getTasksWithinImportanceRange range = get >>= pure . C.getTasksMatching ((`member` range) . (.importance))
