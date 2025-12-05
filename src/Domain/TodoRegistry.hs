module Domain.TodoRegistry
    ( TodoRegistry
    , TaskId
    , Task
    , TaskStatus
    , TaskDetail (..)
    , TaskBasic (..)
    , TaskStatusDetail (..)
    , EntryCreate (..)
    , EntryPatch (..)
    , PatchStatus (..)
    -- TodoRegistry Construction
    , initTodoRegistry
    -- TodoRegistry Query
    , isIdInUse
    , getTaskById
    , getAllTasks
    , getDoneTasks
    , getDueTasks
    , getOverdueTasks
    , getUndoneTasks
    , getTasksByNameRegex
    , getTasksWithAllTags
    -- TodoRegistry Update/Delete
    , insertTask
    , updateTask
    , deleteTask
    -- Todo Construction
    , mkTask
    -- Todo Update
    , modifyTask
    -- Todo Conversion
    , toTaskDetail
    , toTaskBasic
    ) where

import Data.Fixed (Pico)
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.HashSet (HashSet)
import Data.Hashable (Hashable (..))
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime)
import Data.Time.LocalTime (TimeZone, utcToLocalTime)
import GHC.Generics (Generic)
import Text.Regex.TDFA (Regex, makeRegex, matchTest)
import Witch

import Data.HashSet qualified as S
import Data.IntMap qualified as IM
import Data.Map qualified as M

import Domain.Error
import Domain.TaskId

data TodoRegistry = TodoRegistry
    { ids :: !Ids
    , idToTask :: !(IntMap Task)
    , tagToId :: !(Map Text (HashSet TaskId))
    , statusToId :: !(Map TaskStatus (HashSet TaskId))
    }
    deriving (Generic, Show)

data Task = Task
    { name :: !Text
    , desc :: !Text
    , tags :: !(HashSet Text)
    , status :: !TaskStatus
    , deadline :: !UTCTime
    }
    deriving (Generic, Show)

data TaskStatus
    = Done
    | Undone
    deriving (Eq, Generic, Ord, Show)

instance Hashable TaskStatus where
    hashWithSalt s Done = s `hashWithSalt` (0 :: Int)
    hashWithSalt s Undone = s `hashWithSalt` (1 :: Int)

data EntryCreate = EntryCreate
    { name :: !Text
    , desc :: !Text
    , tags :: !(HashSet Text)
    , deadline :: !UTCTime
    }
    deriving (Show)

data EntryPatch = EntryPatch
    { name :: !(Maybe Text)
    , desc :: !(Maybe Text)
    , tags :: !(Maybe (HashSet Text))
    , deadline :: !(Maybe UTCTime)
    , status :: !(Maybe PatchStatus)
    }
    deriving (Show)

data PatchStatus
    = PDone
    | PUndone

instance Show PatchStatus where
    show PDone = "done"
    show PUndone = "undone"

instance From PatchStatus TaskStatus where
    from PDone = Done
    from PUndone = Undone

instance From TaskStatus PatchStatus where
    from Done = PDone
    from Undone = PUndone

data TaskDetail = TaskDetail
    { name :: !Text
    , desc :: !Text
    , tags :: !(HashSet Text)
    , status :: !TaskStatusDetail
    , deadline :: !UTCTime
    }
    deriving (Show)

data TaskStatusDetail
    = DDone
    | DUndone
    | DDue
    | DOverdue
    deriving (Eq, Ord)

instance Show TaskStatusDetail where
    show DDone = "done"
    show DUndone = "undone"
    show DDue = "due"
    show DOverdue = "overdue"

instance From TaskStatusDetail Text where
    from = into . show

data TaskBasic = TaskBasic
    { name :: Text
    , desc :: Text
    , tags :: HashSet Text
    }
    deriving (Show)

initTodoRegistry :: TodoRegistry
initTodoRegistry = TodoRegistry initIds IM.empty M.empty M.empty

isIdInUse :: TaskId -> TodoRegistry -> Bool
isIdInUse tid TodoRegistry{idToTask} = IM.member (into tid) idToTask

insertTask :: Task -> TodoRegistry -> TodoRegistry
insertTask task@Task{..} TodoRegistry{..} =
    TodoRegistry
        { ids = ids'
        , idToTask = IM.insert (into tid) task idToTask
        , tagToId = insertIntoSetsAtKeysWith (<>) tid tags tagToId
        , statusToId = insertIntoSetsAtKeysWith (<>) tid (Just status) statusToId
        }
  where
    (ids', tid) = allocId ids

updateTask :: TaskId -> Task -> TodoRegistry -> TodoRegistry
updateTask tid newTask reg@TodoRegistry{..} = case getTaskById tid reg of
    Nothing -> reg
    Just oldTask ->
        TodoRegistry
            { ids = ids
            , idToTask = IM.insert (into tid) newTask idToTask
            , tagToId = getNewTagToId oldTask.tags newTask.tags
            , statusToId = getNewStatusToId oldTask.status newTask.status
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
                & M.map (S.delete tid)
                & M.insertWith (<>) (into newStatus) (S.singleton tid)

deleteTask :: TaskId -> TodoRegistry -> TodoRegistry
deleteTask tid reg@TodoRegistry{..}
    | Just Task{tags, status} <- getTaskById tid reg =
        TodoRegistry
            { ids = releaseId tid ids
            , idToTask = IM.delete (into tid) idToTask
            , tagToId = deleteFromSetsAtKeys tid tags tagToId
            , statusToId = deleteFromSetsAtKeys tid (Just status) statusToId
            }
    | otherwise = reg

getTaskById :: TaskId -> TodoRegistry -> Maybe Task
getTaskById tid TodoRegistry{idToTask} = IM.lookup (into tid) idToTask

mkTask :: TimeZone -> UTCTime -> EntryCreate -> Either DomainError Task
mkTask tz now EntryCreate{..}
    | Left e <- validateDeadline tz now deadline = Left e
    | otherwise = Right Task{name, deadline, desc, tags, status = Undone}

modifyTask
    :: TimeZone -> UTCTime -> EntryPatch -> Task -> Either DomainError Task
modifyTask tz now entry task
    | Just d <- entry.deadline, Left e <- validateDeadline tz now d = Left e
    | otherwise =
        Right
            Task
                { name = fromMaybe task.name entry.name
                , desc = fromMaybe task.desc entry.desc
                , tags = fromMaybe task.tags entry.tags
                , deadline = fromMaybe task.deadline entry.deadline
                , status = maybe task.status into entry.status
                }

toTaskDetail :: UTCTime -> Task -> TaskDetail
toTaskDetail now Task{..} =
    TaskDetail{name, desc, deadline, tags, status = enrichStatus status}
  where
    enrichStatus :: TaskStatus -> TaskStatusDetail
    enrichStatus Done = DDone
    enrichStatus Undone
        | isOverdue now deadline = DOverdue
        | isDue now deadline = DDue
        | otherwise = DUndone

toTaskBasic :: Task -> TaskBasic
toTaskBasic Task{..} = TaskBasic{name, desc, tags}

getAllTasks :: TodoRegistry -> HashSet TaskId
getAllTasks = getTasksMatching (const True)

getDoneTasks :: TodoRegistry -> HashSet TaskId
getDoneTasks = getTasksByStatus Done

getUndoneTasks :: TodoRegistry -> HashSet TaskId
getUndoneTasks = getTasksByStatus Undone

getOverdueTasks :: UTCTime -> TodoRegistry -> HashSet TaskId
getOverdueTasks now = getTasksUndoneAnd $ isOverdue now

getDueTasks :: UTCTime -> TodoRegistry -> HashSet TaskId
getDueTasks now = getTasksUndoneAnd $ isDue now

getTasksByNameRegex :: Text -> TodoRegistry -> HashSet TaskId
getTasksByNameRegex pattern = getTasksMatching (\Task{name} -> matchTest compiled name)
  where
    compiled = makeRegex pattern :: Regex

getTasksWithAllTags :: HashSet Text -> TodoRegistry -> HashSet TaskId
getTasksWithAllTags tags reg@TodoRegistry{tagToId}
    | S.null tags = getAllTasks reg
    | otherwise =
        tags
            & S.map (\t -> M.findWithDefault S.empty t tagToId & Intersection)
            & foldr1 (<>)
            & into

-- private

newtype Intersection a = Intersection {getIntersection :: HashSet a}
    deriving stock (Eq, Show)
    deriving newtype (Hashable)

instance (Eq a, Hashable a) => Semigroup (Intersection a) where
    Intersection x <> Intersection y = Intersection (S.intersection x y)

instance From (Intersection a) (HashSet a)

instance From (HashSet a) (Intersection a)

statusDueThreshold :: NominalDiffTime
statusDueThreshold = from @Pico $ 48 * 3600

getTasksMatching :: (Task -> Bool) -> TodoRegistry -> HashSet TaskId
getTasksMatching p TodoRegistry{idToTask} = IM.foldrWithKey insertKeyWhen S.empty idToTask
  where
    insertKeyWhen :: Int -> Task -> HashSet TaskId -> HashSet TaskId
    insertKeyWhen key task set
        | p task = S.insert (into key) set
        | otherwise = set

getTasksByStatus :: TaskStatus -> TodoRegistry -> HashSet TaskId
getTasksByStatus s TodoRegistry{statusToId} = M.findWithDefault S.empty s statusToId

getTasksUndoneAnd :: (UTCTime -> Bool) -> TodoRegistry -> HashSet TaskId
getTasksUndoneAnd p reg@TodoRegistry{idToTask} =
    getUndoneTasks reg
        & S.filter (maybe False (\Task{deadline} -> p deadline) . (idToTask IM.!?) . into)

isDue :: UTCTime -> UTCTime -> Bool
isDue now = liftA2 (&&) (> now) (<= addUTCTime statusDueThreshold now)

isOverdue :: UTCTime -> UTCTime -> Bool
isOverdue now = (<= now)

validateDeadline :: TimeZone -> UTCTime -> UTCTime -> Either DomainError ()
validateDeadline tz now dd
    | dd > now = Right ()
    | otherwise =
        let
            nowL = utcToLocalTime tz now
            ddL = utcToLocalTime tz dd
         in
            Left $ InvalidDeadline nowL ddL

deleteFromSetsAtKeys
    :: (Foldable t, Hashable a, Ord k)
    => a -> t k -> Map k (HashSet a) -> Map k (HashSet a)
deleteFromSetsAtKeys = flip . foldr . M.adjust . S.delete

insertIntoSetsAtKeysWith
    :: (Foldable t, Hashable a, Ord k)
    => (HashSet a -> HashSet a -> HashSet a)
    -> a
    -> t k
    -> Map k (HashSet a)
    -> Map k (HashSet a)
insertIntoSetsAtKeysWith f val ks m = foldr (\k -> M.insertWith f k (S.singleton val)) m ks
