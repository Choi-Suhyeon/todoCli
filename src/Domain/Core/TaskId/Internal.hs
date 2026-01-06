module Domain.Core.TaskId.Internal
    ( TaskId
    , Ids
    , initIds
    , allocId
    , releaseId
    ) where

import Data.IntSet qualified as IS

import Common.Prelude
import Domain.Core.TaskId.Raw

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
