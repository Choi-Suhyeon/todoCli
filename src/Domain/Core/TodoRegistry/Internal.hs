module Domain.Core.TodoRegistry.Internal
    ( TodoRegistry (..)

      -- * Public Construction
    , initTodoRegistry

      -- * Internal(Domain) Query
    , isIdInUse
    , getTaskById
    , getAllTasks
    , getDoneTasks
    , getDueTasks
    , getOverdueTasks
    , getUndoneTasks
    , getTasksByNameRegex
    , getTasksWithAllTags

      -- * Internal(Domain) Update/Delete
    , insertTask
    , replaceTask
    , deleteTask
    ) where

import Data.HashSet (HashSet)
import Data.Hashable (Hashable (..))
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Serialize (Serialize (..))
import Data.Text (Text)
import Data.Time.Clock (UTCTime (..))

import Data.HashSet qualified as HS
import Data.IntMap qualified as IM
import Data.Map qualified as M

import Common
import Common.Prelude hiding (get, put)
import Domain.Core.Internal
import Domain.Core.Task
import Domain.Core.Task.Internal (Task, TaskStatus)
import Domain.Core.TaskId
import Domain.Error
import Common.Serialization.CerealOrphans ()

data TodoRegistry = TodoRegistry
    { ids :: !Ids
    , idToTask :: !(IntMap Task)
    , tagToId :: !(Map Text (HashSet TaskId))
    , statusToId :: !(Map TaskStatus (HashSet TaskId))
    }
    deriving (Show)

instance Serialize TodoRegistry where
    put TodoRegistry{..} = do
        put ids
        put idToTask
        put tagToId
        put statusToId

    get = TodoRegistry <$> get <*> get <*> get <*> get

initTodoRegistry :: TodoRegistry
initTodoRegistry = TodoRegistry initIds IM.empty M.empty M.empty

isIdInUse :: TaskId -> TodoRegistry -> Bool
isIdInUse tid TodoRegistry{idToTask} = IM.member (into tid) idToTask

insertTask :: Task -> TodoRegistry -> Either DomainError TodoRegistry
insertTask task TodoRegistry{..} =
    maybeToEither TaskIdExhausted
        $ allocId ids
        <&> \(ids', tid) ->
            TodoRegistry
                { ids = ids'
                , idToTask = IM.insert (into tid) task idToTask
                , tagToId = insertIntoSetsAtKeysWith (<>) tid (into @(HashSet Text) tags) tagToId
                , statusToId = insertIntoSetsAtKeysWith (<>) tid (Just $ into status) statusToId
                }
  where
    TaskBasic{tags, status} = toTaskBasic task

replaceTask :: TaskId -> Task -> TodoRegistry -> TodoRegistry
replaceTask tid newTask reg@TodoRegistry{..} = case getTaskById tid reg of
    Nothing -> reg
    Just oldTask ->
        TodoRegistry
            { ids = ids
            , idToTask = IM.insert (into tid) newTask idToTask
            , tagToId = (getNewTagToId `on` (.tags) . toTaskBasic) oldTask newTask
            , statusToId = (getNewStatusToId `on` (.status) . toTaskBasic) oldTask newTask
            }
  where
    getNewTagToId oldTags newTags
        | oldTags == newTags = tagToId
        | otherwise =
            tagToId
                & deleteFromSetsAtKeys tid oldTags
                & insertIntoSetsAtKeysWith (<>) tid newTags

    getNewStatusToId oldStatus newStatus
        | oldStatus == newStatus = statusToId
        | otherwise =
            statusToId
                & M.map (HS.delete tid)
                & M.insertWith (<>) (into newStatus) (HS.singleton tid)

deleteTask :: TaskId -> TodoRegistry -> TodoRegistry
deleteTask tid reg@TodoRegistry{..}
    | Just task <- getTaskById tid reg
    , let
        TaskBasic{tags, status} = toTaskBasic task =
        TodoRegistry
            { ids = releaseId tid ids
            , idToTask = IM.delete (into tid) idToTask
            , tagToId = deleteFromSetsAtKeys tid (into @(HashSet Text) tags) tagToId
            , statusToId = deleteFromSetsAtKeys tid (Just $ into status) statusToId
            }
    | otherwise = reg

getTaskById :: TaskId -> TodoRegistry -> Maybe Task
getTaskById tid TodoRegistry{idToTask} = IM.lookup (into tid) idToTask

getAllTasks :: TodoRegistry -> HashSet TaskId
getAllTasks = getTasksMatching (const True)

getDoneTasks :: TodoRegistry -> HashSet TaskId
getDoneTasks = getTasksByStatus $ into BDone

getUndoneTasks :: TodoRegistry -> HashSet TaskId
getUndoneTasks = getTasksByStatus $ into BUndone

getOverdueTasks :: UTCTime -> TodoRegistry -> HashSet TaskId
getOverdueTasks now = getTasksUndoneAnd $ isOverdue now

getDueTasks :: UTCTime -> TodoRegistry -> HashSet TaskId
getDueTasks now = getTasksUndoneAnd $ isDue now

getTasksByNameRegex :: Text -> TodoRegistry -> HashSet TaskId
getTasksByNameRegex pattern = getTasksMatching (\TaskBasic{name} -> matchTest compiled (into @Text name))
  where
    compiled = makeRegex pattern :: Regex

getTasksWithAllTags :: HashSet Text -> TodoRegistry -> HashSet TaskId
getTasksWithAllTags tags reg@TodoRegistry{tagToId}
    | HS.null tags = getAllTasks reg
    | otherwise =
        tags
            & HS.map (\t -> M.findWithDefault HS.empty t tagToId & Intersection)
            & foldr1 (<>)
            & into

-- private

newtype Intersection a = Intersection {getIntersection :: HashSet a}
    deriving stock (Eq, Show)
    deriving newtype (Hashable)

instance (Eq a, Hashable a) => Semigroup (Intersection a) where
    Intersection x <> Intersection y = Intersection (HS.intersection x y)

instance From (Intersection a) (HashSet a)

instance From (HashSet a) (Intersection a)

getTasksMatching :: (TaskBasic -> Bool) -> TodoRegistry -> HashSet TaskId
getTasksMatching predicate TodoRegistry{idToTask} =
    IM.foldrWithKey insertKeyWhen HS.empty idToTask
  where
    insertKeyWhen :: Int -> Task -> HashSet TaskId -> HashSet TaskId
    insertKeyWhen key task set
        | predicate $ toTaskBasic task = HS.insert (into key) set
        | otherwise = set

getTasksByStatus :: TaskStatus -> TodoRegistry -> HashSet TaskId
getTasksByStatus s TodoRegistry{statusToId} = M.findWithDefault HS.empty s statusToId

getTasksUndoneAnd :: (UTCTime -> Bool) -> TodoRegistry -> HashSet TaskId
getTasksUndoneAnd p reg@TodoRegistry{idToTask} =
    getUndoneTasks reg
        & HS.filter (predicate . (idToTask IM.!?) . into)
  where
    predicate :: Maybe Task -> Bool
    predicate task = case deadline of
        Just (BBound x) -> p x
        Just BBoundless -> False
        Nothing -> False
      where
        deadline :: Maybe TaskBasicDeadline
        deadline = (.deadline) . toTaskBasic <$> task

deleteFromSetsAtKeys
    :: (Foldable t, Hashable a, Ord k)
    => a -> t k -> Map k (HashSet a) -> Map k (HashSet a)
deleteFromSetsAtKeys = flip . foldr . M.adjust . HS.delete

insertIntoSetsAtKeysWith
    :: (Foldable t, Hashable a, Ord k)
    => (HashSet a -> HashSet a -> HashSet a)
    -> a
    -> t k
    -> Map k (HashSet a)
    -> Map k (HashSet a)
insertIntoSetsAtKeysWith f val ks m = foldr (\k -> M.insertWith f k (HS.singleton val)) m ks
