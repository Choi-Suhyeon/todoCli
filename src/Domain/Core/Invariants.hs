module Domain.Core.Invariants
    ( checkIdInvariant
    , checkTagIndexInvariant
    , checkStatusIndexInvariant
    , rebuildIds
    ) where

import Data.HashSet qualified as HS
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.Map qualified as M

import Domain.Core.Task.Raw
import Domain.Core.TaskId.Raw
import Domain.Core.TodoRegistry.Internal
import Domain.Core.TodoRegistry.Raw
import Domain.Error
import External.Prelude

checkIdInvariant :: TodoRegistry -> Bool
checkIdInvariant TodoRegistry{..}
    | IS.null keySets = True
    | otherwise =
        and @[]
            [ IS.findMin keySets >= into (minBound :: TaskId)
            , IS.findMax keySets < into (ids.next)
            , IS.null $ IS.intersection keySets ids.released
            ]
  where
    keySets = IS.fromList $ IM.keys idToTask

checkTagIndexInvariant :: TodoRegistry -> Bool
checkTagIndexInvariant TodoRegistry{..} =
    and
        $ IM.mapWithKey
            (\k v -> go (into k) v.tags)
            idToTask
  where
    go :: TaskId -> TaskTags -> Bool
    go tid tags =
        tagToId
            & M.filter (HS.member tid)
            & M.keys
            & HS.fromList
            & (== into tags)

checkStatusIndexInvariant :: TodoRegistry -> Bool
checkStatusIndexInvariant TodoRegistry{..} =
    and
        $ IM.mapWithKey
            (\k v -> go (into k) v.status)
            idToTask
  where
    go :: TaskId -> TaskStatus -> Bool
    go tid status =
        statusToId
            & M.filter (HS.member tid)
            & M.keys
            & HS.fromList
            & (== HS.singleton status)

rebuildIds :: TodoRegistry -> Either DomainError TodoRegistry
rebuildIds = foldM (flip insertTask) initTodoRegistry . IM.elems . (.idToTask)
