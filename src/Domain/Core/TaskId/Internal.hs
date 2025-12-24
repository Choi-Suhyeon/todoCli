module Domain.Core.TaskId.Internal
    ( TaskId (..)
    , Ids (..)
    , initIds
    , allocId
    , releaseId
    ) where

import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.IntSet (IntSet)
import Data.Serialize (Get, Serialize (..))

import Data.IntSet qualified as IS

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

initIds :: Ids
initIds = Ids minBound IS.empty

allocId :: Ids -> Maybe (Ids, TaskId)
allocId ids
    | not $ IS.null ids.released =
        let
            (i, rel') = IS.deleteFindMin ids.released
         in
            Just (ids{released = rel'}, TaskId i)
    | let
        tid = ids.next
    , tid < maxBound =
        Just (ids{next = succ tid}, tid)
    | otherwise =
        Nothing

releaseId :: TaskId -> Ids -> Ids
releaseId tid ids
    | not isValidId = ids
    | isDoubleFreed = ids
    | isMaxId = ids{next = pred ids.next}
    | otherwise = ids{released = IS.insert (into tid) ids.released}
  where
    isValidId, isMaxId, isDoubleFreed :: Bool

    isValidId = minBound <= tid && tid < ids.next
    isMaxId = ids.next > minBound && succ tid == ids.next
    isDoubleFreed = tid.unTaskId `IS.member` ids.released
