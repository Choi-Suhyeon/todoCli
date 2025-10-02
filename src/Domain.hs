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
import Data.Maybe (catMaybes)
import Data.Semigroup (Semigroup (..))
import Data.Text (Text)
import Data.Time.Clock (UTCTime, addUTCTime)

import Data.HashSet qualified as S
import Data.IntMap qualified as IM
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Text qualified as T

import Common
import Domain.Error
import Domain.Internal
import Domain.Type
import Domain.Type.Internal

data TaskSnapshot
    = TaskSnapshot
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

data EntryCreate
    = EntryCreate
    { name :: !Text
    , desc :: !Text
    , tags :: !(HashSet Text)
    , deadline :: !UTCTime
    }
    deriving (Generic, Show)

data EntryUpdate
    = EntryUpdate
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
            & S.toList
            & map (Intersection . flip (M.findWithDefault S.empty) tagToId)
            & NE.nonEmpty
            & maybe S.empty ((^. #getIntersection) . sconcat)

getTasksByNameContaining :: (MonadRegistry m) => Text -> m (HashSet TaskId)
getTasksByNameContaining subs = getTasksMatching ((subs `isInfixOfI`) . (^. #name))
  where
    isInfixOfI :: Text -> Text -> Bool
    isInfixOfI = T.isInfixOf `on` T.toCaseFold

getDoneTasks :: (MonadRegistry m) => m (HashSet TaskId)
getDoneTasks = getTasksByStatus Done

getUndoneTasks :: (MonadRegistry m) => m (HashSet TaskId)
getUndoneTasks = getTasksByStatus Undone

getOverdueTasks :: (MonadEnv m, MonadRegistry m) => m (HashSet TaskId)
getOverdueTasks = do
    now <- asks (^. #now)
    undoneIds <- getUndoneTasks
    TodoRegistry{idToTask} <- get

    let
        isOverdueTask (TaskId tid) = maybe False ((<= now) . (^. #deadline)) $ idToTask IM.!? tid

    pure $ S.filter isOverdueTask undoneIds

getDueTasks :: (MonadEnv m, MonadRegistry m) => m (HashSet TaskId)
getDueTasks = do
    now <- asks (^. #now)
    undoneIds <- getUndoneTasks
    TodoRegistry{idToTask} <- get

    let
        isDueTask (TaskId tid) = maybe False (isDue now . (^. #deadline)) $ idToTask IM.!? tid

    pure $ S.filter isDueTask undoneIds

addTask
    :: (MonadDomainError e m, MonadEnv m, MonadRegistry m) => EntryCreate -> m ()
addTask e = do
    Env{..} <- ask
    (ids', tid) <- gets (allocId . (^. #ids))

    liftEitherFrom $ validateDeadline tz now e.deadline

    modify'
        $ (#ids .~ ids')
            . (#statusToId %~ insertIntoMapSetKeysWith (<>) tid (Undone :| []))
            . (#tagToId %~ insertIntoMapSetKeysWith (<>) tid e.tags)
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
    => EntryUpdate -> TaskId -> m ()
editTask e tid = do
    Env{..} <- ask

    liftEitherFrom $ maybe (pure ()) (validateDeadline tz now) e.deadline

    gets ((IM.!? tid.unTaskId) . (^. #idToTask)) >>= \case
        Nothing -> throwErrorFrom TaskNotFound
        Just old ->
            modify'
                $ (#idToTask %~ IM.insert tid.unTaskId newTask)
                    . (#tagToId %~ maybe id updateTags e.tags)
          where
            newTask =
                old
                    & updateIfJust #name #name e
                    . updateIfJust #desc #desc e
                    . updateIfJust #tags #tags e
                    . updateIfJust #deadline #deadline e

            updateTags ts =
                insertIntoMapSetKeysWith (<>) tid ts
                    . deleteFromMapSetKeys tid old.tags

markTask :: (MonadRegistry m) => MarkStatus -> TaskId -> m ()
markTask s tid = do
    exists <- gets $ IM.member tid.unTaskId . (^. #idToTask)

    let
        s' = markStatusToTaskStatus s

    when exists . modify'
        $ (#idToTask %~ IM.adjust (#status .~ s') tid.unTaskId)
            . (#statusToId %~ M.insertWith S.union s' (S.singleton tid) . M.map (S.delete tid))

deleteTasks :: (MonadRegistry m) => HashSet TaskId -> m ()
deleteTasks = modify' . flip (foldl' go)
  where
    go :: TodoRegistry -> TaskId -> TodoRegistry
    go original@TodoRegistry{..} tid = go' original
      where
        go' =
            idToTask IM.!? tid.unTaskId & maybe id \Task{tags, status} x ->
                x
                    & #tagToId
                    %~ deleteFromMapSetKeys tid tags
                    & #statusToId
                    %~ deleteFromMapSetKeys tid (status :| [])
                    & #idToTask
                    %~ IM.delete tid.unTaskId
                    & #ids
                    %~ releaseId tid

getTaskSnapshots
    :: (MonadEnv m, MonadRegistry m) => HashSet TaskId -> m [TaskSnapshot]
getTaskSnapshots tids = do
    TodoRegistry{idToTask} <- get
    env <- ask

    pure
        $ catMaybes
            . map
                ( (fromTaskToSnapshot env <$>)
                    . (idToTask IM.!?)
                    . (^. #unTaskId)
                )
            . S.toList
        $ tids
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

markStatusToTaskStatus :: MarkStatus -> TaskStatus
markStatusToTaskStatus MDone = Done
markStatusToTaskStatus MUndone = Undone
