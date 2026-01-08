module Domain.Core.TodoRegistry.Internal
    ( TodoRegistry

      -- * Public Construction
    , initTodoRegistry

      -- * Internal(Domain) Query
    , isIdInUse
    , getTaskById
    , getDoneTasks
    , getUndoneTasks
    , getTasksUndoneAnd
    , getTasksWithAllTags
    , getTasksMatching

      -- * Internal(Domain) Update/Delete
    , insertTask
    , replaceTask
    , deleteTask
    ) where

import Data.HashSet (HashSet)
import Data.Hashable (Hashable (..))
import Data.Map (Map)
import Data.Text (Text)
import Data.Time.Clock (UTCTime (..))

import Data.HashSet qualified as HS
import Data.IntMap qualified as IM
import Data.Map qualified as M

import Common.Prelude
import Common.Serialization.CerealOrphans ()
import Domain.Core.Task.Internal
import Domain.Core.TaskId.Internal
import Domain.Core.TodoRegistry.Raw
import Domain.Error

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

getDoneTasks :: TodoRegistry -> HashSet TaskId
getDoneTasks = getTasksByStatus $ into BDone

getUndoneTasks :: TodoRegistry -> HashSet TaskId
getUndoneTasks = getTasksByStatus $ into BUndone

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

getTasksWithAllTags :: HashSet Text -> TodoRegistry -> HashSet TaskId
getTasksWithAllTags tags reg@TodoRegistry{tagToId}
    | HS.null tags = getTasksMatching (const True) reg
    | otherwise =
        tags
            & HS.map (\t -> M.findWithDefault HS.empty t tagToId)
            & foldr1 HS.intersection
            & into

getTasksMatching :: (TaskBasic -> Bool) -> TodoRegistry -> HashSet TaskId
getTasksMatching predicate TodoRegistry{idToTask} =
    IM.foldrWithKey insertKeyWhen HS.empty idToTask
  where
    insertKeyWhen :: Int -> Task -> HashSet TaskId -> HashSet TaskId
    insertKeyWhen key task set
        | predicate $ toTaskBasic task = HS.insert (into key) set
        | otherwise = set

-- private
getTasksByStatus :: TaskStatus -> TodoRegistry -> HashSet TaskId
getTasksByStatus s TodoRegistry{statusToId} = M.findWithDefault HS.empty s statusToId

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
