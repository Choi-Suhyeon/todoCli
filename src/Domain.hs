module Domain
    ( module Domain.Error
    , module Domain.Type
    , TaskSnapshot (..)
    , EntryCreate (..)
    , EntryUpdate (..)
    , SnapshotStatus (..)
    , MarkStatus (..)
    , getAllTasks
    , getTasksWithAllTags
    , getTasksByNameContaining
    , getDoneTasks
    , getUndoneTasks
    , getOverdueTasks
    , getDueTasks
    , addTask
    , editTask
    , markTask
    , deleteTasks
    , getTaskSnapshots
    ) where

import Control.Monad (when)
import Control.Monad.Reader (ask, asks)
import Control.Monad.State.Strict (MonadState (..), gets, modify')
import Data.Function (on)
import Data.HashSet (HashSet)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, addUTCTime)
import Witch

import Data.HashSet qualified as S
import Data.IntMap qualified as IM
import Data.Map qualified as M
import Data.Text qualified as T

import Common
import Domain.Error
import Domain.Internal
import Domain.Type
import Domain.Type.Internal

data TaskSnapshot = TaskSnapshot
    { nameS :: !Text
    , descS :: !Text
    , tagsS :: !(HashSet Text)
    , statusS :: !SnapshotStatus
    , deadlineS :: !UTCTime
    }
    deriving (Generic, Show)

data SnapshotStatus
    = SDone
    | SUndone
    | SDue
    | SOverdue
    deriving (Eq, Generic, Ord, Show)

data MarkStatus
    = MDone
    | MUndone
    deriving (Generic, Show)

instance From MarkStatus TaskStatus where
    from MDone = Done
    from MUndone = Undone

instance From TaskStatus MarkStatus where
    from Done = MDone
    from Undone = MUndone

data EntryCreate = EntryCreate
    { name :: !Text
    , desc :: !Text
    , tags :: !(HashSet Text)
    , deadline :: !UTCTime
    }
    deriving (Generic, Show)

data EntryUpdate = EntryUpdate
    { name :: !(Maybe Text)
    , desc :: !(Maybe Text)
    , tags :: !(Maybe (HashSet Text))
    , deadline :: !(Maybe UTCTime)
    }
    deriving (Generic, Show)

getAllTasks :: (MonadRegistry m) => m (HashSet TaskId)
getAllTasks = getTasksMatching (const True)

getTasksWithAllTags :: (MonadRegistry m) => HashSet Text -> m (HashSet TaskId)
getTasksWithAllTags tags
    | S.null tags = getAllTasks
    | otherwise = gets \TodoRegistry{tagToId} ->
        tags
            & S.map (\t -> M.findWithDefault S.empty t tagToId & Intersection)
            & foldr1 (<>)
            & into

getTasksByNameContaining :: (MonadRegistry m) => Text -> m (HashSet TaskId)
getTasksByNameContaining subs = getTasksMatching ((subs `isInfixOfI`) . (^. #name))
  where
    isInfixOfI :: Text -> Text -> Bool
    isInfixOfI = T.isInfixOf `on` T.toCaseFold

getDoneTasks :: (MonadRegistry m) => m (HashSet TaskId)
getDoneTasks = getTasksByStatus Done

getUndoneTasks :: (MonadRegistry m) => m (HashSet TaskId)
getUndoneTasks = getTasksByStatus Undone

getTasksUndoneAnd :: (MonadRegistry m) => (UTCTime -> Bool) -> m (HashSet TaskId)
getTasksUndoneAnd p = do
    TodoRegistry{idToTask} <- get

    getUndoneTasks <&> S.filter (maybe False (p . (^. #deadline)) . (idToTask IM.!?) . into)

getOverdueTasks :: (MonadEnv m, MonadRegistry m) => m (HashSet TaskId)
getOverdueTasks = asks (^. #now) >>= getTasksUndoneAnd . isOverdue

getDueTasks :: (MonadEnv m, MonadRegistry m) => m (HashSet TaskId)
getDueTasks = asks (^. #now) >>= getTasksUndoneAnd . isDue

addTask :: (MonadDomainError e m, MonadEnv m, MonadRegistry m) => EntryCreate -> m ()
addTask e = do
    Env{..} <- ask
    (ids', tid) <- gets (allocId . (^. #ids))

    liftEitherInto $ validateDeadline tz now e.deadline

    modify'
        $ (#ids .~ ids')
            . (#statusToId %~ insertIntoSetsAtKeysWith (<>) tid (Undone :| []))
            . (#tagToId %~ insertIntoSetsAtKeysWith (<>) tid e.tags)
            . (#idToTask %~ IM.insert tid.unTaskId newTask)
  where
    newTask =
        Task
            { name = e.name
            , desc = e.desc
            , tags = e.tags
            , deadline = e.deadline
            , status = Undone
            }

editTask
    :: (MonadDomainError e m, MonadEnv m, MonadRegistry m)
    => EntryUpdate
    -> TaskId
    -> m ()
editTask e tid = do
    Env{..} <- ask

    liftEitherInto $ maybe (pure ()) (validateDeadline tz now) e.deadline

    gets ((IM.!? from tid) . (^. #idToTask)) >>= \case
        Nothing -> throwErrorInto TaskNotFound
        Just old ->
            modify'
                $ (#idToTask %~ IM.insert tid.unTaskId newTask)
                    . (#tagToId %~ maybe id updateTags e.tags)
          where
            newTask =
                old
                    & updateIfJust #deadline #deadline e
                    & updateIfJust #name #name e
                    & updateIfJust #desc #desc e
                    & updateIfJust #tags #tags e

            updateTags ts =
                insertIntoSetsAtKeysWith (<>) tid ts . deleteFromSetsAtKeys tid old.tags

markTask :: (MonadRegistry m) => MarkStatus -> TaskId -> m ()
markTask s tid = do
    exists <- gets $ IM.member tid.unTaskId . (^. #idToTask)

    when exists . modify'
        $ (#idToTask %~ IM.adjust (#status .~ into s) tid.unTaskId)
            . (#statusToId %~ M.insertWith S.union (into s) (S.singleton tid) . M.map (S.delete tid))

deleteTasks :: (MonadRegistry m) => HashSet TaskId -> m ()
deleteTasks = modify' . flip (S.foldl' go)
  where
    go :: TodoRegistry -> TaskId -> TodoRegistry
    go original@TodoRegistry{..} tid = go' original
      where
        go' =
            idToTask IM.!? into tid & maybe id \Task{tags, status} x ->
                x
                    & #tagToId
                    %~ deleteFromSetsAtKeys tid tags
                    & #statusToId
                    %~ deleteFromSetsAtKeys tid (status :| [])
                    & #idToTask
                    %~ IM.delete tid.unTaskId
                    & #ids
                    %~ releaseId tid

getTaskSnapshots :: (MonadEnv m, MonadRegistry m) => HashSet TaskId -> m [TaskSnapshot]
getTaskSnapshots tids = do
    TodoRegistry{idToTask} <- get
    env <- ask

    tids
        & S.toList
        & mapMaybe ((fromTaskToSnapshot env <$>) . (idToTask IM.!?) . into)
        & pure
  where
    fromTaskToSnapshot :: Env -> Task -> TaskSnapshot
    fromTaskToSnapshot Env{now} Task{..} =
        TaskSnapshot
            { nameS = name
            , descS = desc
            , tagsS = tags
            , deadlineS = deadline
            , statusS = case status of
                Done -> SDone
                Undone
                    | isDue now deadline -> SDue
                    | isOverdue now deadline -> SOverdue
                    | otherwise -> SUndone
            }

isDue :: UTCTime -> UTCTime -> Bool
isDue now = liftA2 (&&) (> now) (<= addUTCTime statusDueThreshold now)

isOverdue :: UTCTime -> UTCTime -> Bool
isOverdue now = (<= now)
