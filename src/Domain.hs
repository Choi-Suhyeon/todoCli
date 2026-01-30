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
    , C.nameLenBound
    , C.memoLenBound
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

import Control.Arrow ((>>>))
import Data.HashSet (HashSet)
import Data.Text (Text)

import Data.HashSet qualified as HS
import Data.Text qualified as T

import Common
import Domain.Core
    ( EntryCreation (..)
    , EntryDeadline (..)
    , EntryPatch (..)
    , EntryStatus (..)
    , Task
    , TaskBasic (..)
    , TaskId
    , TodoRegistry
    )
import Domain.Error
import Domain.Log
import Domain.TaskDetail
import External.Interval (Interval, member)
import External.Prelude hiding (log)
import External.Regex

import Domain.Core qualified as C

type MonadRegistry m = MonadState TodoRegistry m

addTask
    :: (MonadDomainError e m, MonadEnv m, MonadLog m, MonadRegistry m)
    => EntryCreation -> m ()
addTask e = do
    reg <- get
    Runtime{..} <- asks (.runtime)
    threshold <- asks (.config.dueWithinHours)
    newTask <- liftEitherInto $ C.mkTask tz now e
    reg' <- liftEitherInto $ C.insertTask newTask reg

    let
        taskInfo = renderTaskDetail tz $ toTaskDetail threshold now newTask

    put reg'
    logInfo $ "task added:\n" <> taskInfo

editTask
    :: (MonadDomainError e m, MonadEnv m, MonadLog m, MonadRegistry m)
    => EntryPatch
    -> TaskId
    -> m ()
editTask e tid = do
    Runtime{..} <- asks (.runtime)
    threshold <- asks (.config.dueWithinHours)
    reg <- get

    (oldTask, newTask) <- liftEitherInto do
        old <- maybeToEither TaskNotFound $ C.getTaskById tid reg
        new <- C.modifyTask tz now e old

        pure (old, new)

    let
        msgForOld = renderTaskDetail tz $ toTaskDetail threshold now oldTask
        msgForNew = renderTaskDetail tz $ toTaskDetail threshold now newTask

    logInfo
        $ "task updated:\n  (old)\n"
        <> msgForOld
        <> "\n  (new)\n"
        <> msgForNew
    put $ C.replaceTask tid newTask reg

markTask
    :: (MonadEnv m, MonadLog m, MonadRegistry m) => EntryStatus -> TaskId -> m ()
markTask s tid = do
    Runtime{..} <- asks (.runtime)
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

        logInfo
            $ "task marked "
            <> (show s & into & T.toLower)
            <> ": '"
            <> name
            <> "'"
        put
            $ C.replaceTask tid (fromRight undefined $ C.modifyTask tz now entry task) reg

deleteTasks
    :: (MonadEnv m, MonadLog m, MonadRegistry m) => HashSet TaskId -> m ()
deleteTasks taskIds = do
    Runtime{..} <- asks (.runtime)
    threshold <- asks (.config.dueWithinHours)
    reg <- get

    let
        log :: (MonadLog m) => Maybe Task -> m ()
        log =
            logInfo
                . ("task deleted: " <>)
                . renderTaskSummary
                . (toTaskDetail threshold now)
                . fromJust

    for_ taskIds $ liftA2 when isJust log . (`C.getTaskById` reg)
    put $ HS.foldl' (flip C.deleteTask) reg taskIds

getTaskDetails
    :: (MonadEnv m, MonadRegistry m) => HashSet TaskId -> m [TaskDetail]
getTaskDetails tids = do
    Runtime{..} <- asks (.runtime)
    threshold <- asks (.config.dueWithinHours)
    reg <- get

    HS.toList tids
        & mapMaybe
            ( (`C.getTaskById` reg)
                >>> fmap (toTaskDetail threshold now)
            )
        & pure

getAllTasks :: (MonadRegistry m) => m (HashSet TaskId)
getAllTasks = get >>= pure . C.getTasksMatching (const True)

getDoneTasks :: (MonadRegistry m) => m (HashSet TaskId)
getDoneTasks = get >>= pure . C.getDoneTasks

getUndoneTasks :: (MonadRegistry m) => m (HashSet TaskId)
getUndoneTasks = get >>= pure . C.getUndoneTasks

getOverdueTasks :: (MonadEnv m, MonadRegistry m) => m (HashSet TaskId)
getOverdueTasks =
    asks (.runtime) >>= \Runtime{now} -> get >>= pure . C.getTasksUndoneAnd (isOverdue now)

getDueTasks :: (MonadEnv m, MonadRegistry m) => m (HashSet TaskId)
getDueTasks = do
    now <- asks (.runtime.now)
    threshold <- asks (.config.dueWithinHours)

    get >>= pure . C.getTasksUndoneAnd (isDue threshold now)

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
