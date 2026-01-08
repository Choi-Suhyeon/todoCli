module Domain.Core.Task.Raw
    ( Task (..)
    , TaskName (..)
    , TaskMemo (..)
    , TaskStatus (..)
    , TaskTags (..)
    , TaskDeadline (..)
    , TaskImportance (..)
    ) where

import Data.HashSet (HashSet)
import Data.Hashable (Hashable (..))
import Data.Serialize (Serialize (..), getWord8, putWord8)
import Data.Text (Text)
import Data.Time.Calendar (toModifiedJulianDay)
import Data.Time.Clock (UTCTime (..))

import Common.Prelude hiding (get, put)
import Common.Serialization.CerealOrphans ()

data Task = Task
    { name :: !TaskName
    , memo :: !TaskMemo
    , tags :: !TaskTags
    , status :: !TaskStatus
    , deadline :: !TaskDeadline
    , importance :: !TaskImportance
    }
    deriving (Show)

instance Serialize Task where
    put Task{..} = do
        put name
        put memo
        put tags
        put status
        put deadline
        put importance

    get = Task <$> get <*> get <*> get <*> get <*> get <*> get

newtype TaskName = TaskName {unTaskName :: Text}
    deriving stock (Eq, Ord, Show)
    deriving newtype (Hashable, IsString, Serialize)

instance From Text TaskName
instance From TaskName Text

newtype TaskMemo = TaskMemo {unTaskMemo :: Text}
    deriving stock (Eq, Ord, Show)
    deriving newtype (Hashable, IsString, Serialize)

instance From Text TaskMemo
instance From TaskMemo Text

data TaskStatus = Done | Undone
    deriving (Eq, Ord, Show)

instance Hashable TaskStatus where
    hashWithSalt s Done = s `hashWithSalt` (0 :: Int)
    hashWithSalt s Undone = s `hashWithSalt` (1 :: Int)

instance Serialize TaskStatus where
    put Undone = putWord8 0
    put Done = putWord8 1

    get =
        getWord8 >>= \case
            0 -> pure Undone
            1 -> pure Done
            _ -> fail "TaskStatus: invalid tag"

newtype TaskTags = TaskTags {unTaskTags :: HashSet Text}
    deriving stock (Eq, Ord, Show)
    deriving (Hashable, Semigroup, Serialize) via (HashSet Text)

instance From (HashSet Text) TaskTags
instance From TaskTags (HashSet Text)

data TaskDeadline
    = Boundless
    | Bound UTCTime
    deriving (Eq, Ord, Show)

instance Hashable TaskDeadline where
    hashWithSalt salt Boundless =
        salt `hashWithSalt` (0 :: Int)
    hashWithSalt salt (Bound (UTCTime day diffTime)) =
        salt
            `hashWithSalt` (1 :: Int)
            `hashWithSalt` toModifiedJulianDay day
            `hashWithSalt` (truncate (diffTime * 1000000000) :: Integer)

instance Serialize TaskDeadline where
    put Boundless = putWord8 0
    put (Bound utc) = putWord8 1 *> put utc

    get =
        getWord8 >>= \case
            0 -> pure Boundless
            1 -> Bound <$> get
            _ -> fail "TaskDeadline: invalid tag"

newtype TaskImportance = TaskImportance {unTaskImportance :: Word}
    deriving stock (Eq, Ord, Show)
    deriving newtype (Hashable, Serialize)

instance From Word TaskImportance
instance From TaskImportance Word

instance Bounded TaskImportance where
    minBound = TaskImportance 1
    maxBound = TaskImportance 9
