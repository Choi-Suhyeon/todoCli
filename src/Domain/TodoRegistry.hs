module Domain.TodoRegistry
    ( TodoRegistry
    , TaskId
    , Task
    , TaskName
    , TaskMemo
    , TaskStatus
    , TaskTags
    , TaskDeadline
    , TaskDetail (..)
    , TaskBasic (..)
    , TaskDetailStatus (..)
    , TaskDetailDeadline (..)
    , EntryCreation (..)
    , EntryPatch (..)
    , EntryDeadline (..)
    , EntryStatus (..)
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
    , replaceTask
    , deleteTask
    -- Todo Construction
    , mkTask
    -- Todo Update
    , modifyTask
    -- Todo Conversion
    , toTaskDetail
    , toTaskBasic
    )
where

import Data.Fixed (Pico)
import Data.Function (on, (&))
import Data.Generics.Labels ()
import Data.HashSet (HashSet)
import Data.Hashable (Hashable (..))
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Time.Calendar (toModifiedJulianDay)
import Data.Time.Clock (NominalDiffTime, UTCTime (..), addUTCTime)
import Data.Time.LocalTime (TimeZone, utcToLocalTime)
import GHC.Generics (Generic)
import Text.Regex.TDFA (Regex, makeRegex, matchTest)
import Data.Serialize (Serialize (..))
import Witch

import Data.HashSet qualified as S
import Data.IntMap qualified as IM
import Data.Map qualified as M

import Domain.Serialization.CerealOrphans ()
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
    { name :: !TaskName
    , memo :: !TaskMemo
    , tags :: !TaskTags
    , status :: !TaskStatus
    , deadline :: !TaskDeadline
    }
    deriving (Generic, Show)

newtype TaskName = TaskName {unTaskName :: Text}
    deriving stock (Eq, Generic, Ord, Show)
    deriving (Hashable, IsString) via Text

instance From Text TaskName
instance From TaskName Text

newtype TaskMemo = TaskMemo {unTaskMemo :: Text}
    deriving stock (Eq, Generic, Ord, Show)
    deriving (Hashable, IsString) via Text

instance From Text TaskMemo
instance From TaskMemo Text

data TaskStatus = Done | Undone
    deriving (Eq, Generic, Ord, Show)

instance Hashable TaskStatus where
    hashWithSalt s Done = s `hashWithSalt` (0 :: Int)
    hashWithSalt s Undone = s `hashWithSalt` (1 :: Int)

newtype TaskTags = TaskTags {unTaskTags :: HashSet Text}
    deriving stock (Eq, Generic, Ord, Show)
    deriving (Hashable, Monoid, Semigroup) via (HashSet Text)

instance From (HashSet Text) TaskTags
instance From TaskTags (HashSet Text)

data TaskDeadline
    = Boundless
    | Bound UTCTime
    deriving (Eq, Generic, Ord, Show)

instance Hashable TaskDeadline where
    hashWithSalt salt Boundless =
        salt `hashWithSalt` (0 :: Int)
    hashWithSalt salt (Bound (UTCTime day diffTime)) =
        salt
            `hashWithSalt` (1 :: Int)
            `hashWithSalt` toModifiedJulianDay day
            `hashWithSalt` (truncate (diffTime * 1000000000) :: Integer)

deriving anyclass instance Serialize TaskStatus
deriving anyclass instance Serialize TaskDeadline
deriving anyclass instance Serialize TaskTags
deriving anyclass instance Serialize TaskName
deriving anyclass instance Serialize TaskMemo
deriving anyclass instance Serialize Task
deriving anyclass instance Serialize TodoRegistry

data EntryCreation = EntryCreation
    { name :: !Text
    , memo :: !Text
    , tags :: !(HashSet Text)
    , deadline :: !EntryDeadline
    }
    deriving (Show)

data EntryPatch = EntryPatch
    { name :: !(Maybe Text)
    , memo :: !(Maybe Text)
    , tags :: !(Maybe (HashSet Text))
    , deadline :: !(Maybe EntryDeadline)
    , status :: !(Maybe EntryStatus)
    }
    deriving (Show)

data EntryDeadline
    = EBoundless
    | EBound UTCTime
    deriving (Generic, Show)

instance From EntryDeadline TaskDeadline where
    from EBoundless = Boundless
    from (EBound t) = Bound t

data EntryStatus
    = EDone
    | EUndone

instance Show EntryStatus where
    show EDone = "done"
    show EUndone = "undone"

instance From EntryStatus TaskStatus where
    from EDone = Done
    from EUndone = Undone

instance From TaskStatus EntryStatus where
    from Done = EDone
    from Undone = EUndone

data TaskDetail = TaskDetail
    { name :: !Text
    , memo :: !Text
    , tags :: !(HashSet Text)
    , status :: !TaskDetailStatus
    , deadline :: !TaskDetailDeadline
    }
    deriving (Show)

data TaskDetailStatus
    = DDone
    | DUndone
    | DDue
    | DOverdue
    deriving (Eq, Ord)

instance Show TaskDetailStatus where
    show DDone = "done"
    show DUndone = "undone"
    show DDue = "due"
    show DOverdue = "overdue"

data TaskDetailDeadline
    = DBoundless
    | DBound UTCTime
    deriving (Eq, Show)

instance From TaskDeadline TaskDetailDeadline where
    from Boundless = DBoundless
    from (Bound d) = DBound d

instance Ord TaskDetailDeadline where
    compare DBoundless DBoundless = EQ
    compare DBoundless (DBound _) = GT
    compare (DBound _) DBoundless = LT
    compare (DBound d1) (DBound d2) = d1 `compare` d2

data TaskBasic = TaskBasic
    { name :: Text
    , memo :: Text
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
        , tagToId = insertIntoSetsAtKeysWith (<>) tid (into @(HashSet Text) tags) tagToId
        , statusToId = insertIntoSetsAtKeysWith (<>) tid (Just status) statusToId
        }
  where
    (ids', tid) = allocId ids

replaceTask :: TaskId -> Task -> TodoRegistry -> TodoRegistry
replaceTask tid newTask reg@TodoRegistry{..} = case getTaskById tid reg of
    Nothing -> reg
    Just oldTask ->
        TodoRegistry
            { ids = ids
            , idToTask = IM.insert (into tid) newTask idToTask
            , tagToId = (getNewTagToId `on` into @(HashSet Text)) oldTask.tags newTask.tags
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
            , tagToId = deleteFromSetsAtKeys tid (into @(HashSet Text) tags) tagToId
            , statusToId = deleteFromSetsAtKeys tid (Just status) statusToId
            }
    | otherwise = reg

getTaskById :: TaskId -> TodoRegistry -> Maybe Task
getTaskById tid TodoRegistry{idToTask} = IM.lookup (into tid) idToTask

mkTask :: TimeZone -> UTCTime -> EntryCreation -> Either DomainError Task
mkTask tz now EntryCreation{..}
    | EBound d <- deadline, Left e <- validateDeadline tz now d = Left e
    | otherwise =
        Right
            Task
                { name = into name
                , memo = into memo
                , tags = into tags
                , deadline = into deadline
                , status = Undone
                }

modifyTask
    :: TimeZone -> UTCTime -> EntryPatch -> Task -> Either DomainError Task
modifyTask tz now entry task
    | Just (EBound d) <- entry.deadline
    , Left e <- validateDeadline tz now d =
        Left e
    | otherwise =
        Right
            Task
                { name = fromMaybe task.name $ into <$> entry.name
                , memo = fromMaybe task.memo $ into <$> entry.memo
                , tags = fromMaybe task.tags $ into <$> entry.tags
                , deadline = fromMaybe task.deadline (into <$> entry.deadline)
                , status = maybe task.status into entry.status
                }

toTaskDetail :: UTCTime -> Task -> TaskDetail
toTaskDetail now Task{..} =
    TaskDetail
        { name = into name
        , memo = into memo
        , tags = into tags
        , deadline = into deadline
        , status = enrichStatus status
        }
  where
    enrichStatus :: TaskStatus -> TaskDetailStatus
    enrichStatus Done = DDone
    enrichStatus Undone = case deadline of
        Boundless -> DUndone
        Bound d
            | isOverdue now d -> DOverdue
            | isDue now d -> DDue
            | otherwise -> DUndone

toTaskBasic :: Task -> TaskBasic
toTaskBasic Task{..} =
    TaskBasic
        { name = into name
        , memo = into memo
        , tags = into tags
        }

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
getTasksByNameRegex pattern = getTasksMatching (\Task{name} -> matchTest compiled (into @Text name))
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
        & S.filter (predicate . (idToTask IM.!?) . into)
  where
    predicate :: Maybe Task -> Bool
    predicate (Just Task{deadline = Bound x}) = p x
    predicate (Just Task{deadline = Boundless}) = False
    predicate Nothing = False

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
