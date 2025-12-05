module Domain.TaskId
    ( TaskId (..)
    , Ids
    , initIds
    , allocId
    , releaseId
    ) where

import Data.Bifunctor (bimap)
import Data.Generics.Labels ()
import Data.Hashable (Hashable)
import Data.IntSet (IntSet)
import Data.Tuple (swap)
import Witch

import Data.IntSet qualified as IS

import Common.Optics

newtype TaskId = TaskId {unTaskId :: Int}
    deriving stock (Eq, Generic, Ord, Show)
    deriving newtype (Hashable)

instance From Int TaskId

instance From TaskId Int

minTaskId :: TaskId
minTaskId = TaskId 1

data Ids = Ids
    { next :: !TaskId
    , released :: !IntSet
    }
    deriving (Generic, Show)

initIds :: Ids
initIds = Ids minTaskId IS.empty

allocId :: Ids -> (Ids, TaskId)
allocId ids
    | IS.null ids.released =
        (ids & #next . #unTaskId %~ succ, ids.next)
    | otherwise =
        ids.released
            & IS.deleteFindMin
            & swap
            & bimap (($ ids) . (#released .~)) TaskId

releaseId :: TaskId -> Ids -> Ids
releaseId tid ids
    | isValidId =
        if
            | isDoubleFreed -> ids
            | isMaxId -> ids & #next . #unTaskId %~ pred
            | otherwise -> ids & #released %~ IS.insert tid.unTaskId
    | otherwise = ids
  where
    isValidId, isMaxId, isDoubleFreed :: Bool

    isValidId = liftA2 (&&) (>= minTaskId) (< ids.next) tid
    isMaxId = (tid & #unTaskId %~ succ) == ids.next
    isDoubleFreed = tid.unTaskId `IS.member` ids.released
