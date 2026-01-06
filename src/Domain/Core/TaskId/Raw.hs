module Domain.Core.TaskId.Raw (TaskId (..), Ids (..)) where

import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.IntSet (IntSet)
import Data.Serialize (Get, Serialize (..))

import Common.Prelude hiding (get, put)

newtype TaskId = TaskId {unTaskId :: Int}
    deriving stock (Eq, Ord, Show)
    deriving newtype (Enum, Hashable)

instance From Int TaskId
instance From TaskId Int

instance TryFrom Int64 TaskId where
    tryFrom =
        fmap (into @TaskId)
            . first (\(TryFromException src e) -> TryFromException src e)
            . tryInto @Int

instance Bounded TaskId where
    minBound = TaskId 1
    maxBound = TaskId maxBound

instance Serialize TaskId where
    put tid = put $ via @Int @_ @Int64 tid
    get = do
        tid <- either (const $ failed) pure . tryInto @TaskId =<< get @Int64

        unless (tid >= minBound) failed
        pure tid
      where
        failed :: Get a
        failed = fail "TaskId: invalid id"

data Ids = Ids
    { next :: !TaskId
    , released :: !IntSet
    }
    deriving (Show)

instance Serialize Ids where
    put Ids{..} = put next *> put released
    get = liftA2 Ids get get
