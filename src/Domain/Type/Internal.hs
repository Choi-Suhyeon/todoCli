module Domain.Type.Internal
    ( Intersection (..)
    , TodoRegistry (..)
    , initTodoRegistry
    , Task (..)
    , TaskStatus (..)
    , TaskId (..)
    , minTaskId
    , Ids (..)
    , initIds
    , allocId
    , releaseId
    ) where

import Data.Bifunctor (bimap)
import Data.Generics.Labels ()
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Lens.Micro ((%~), (&), (.~))
import Witch

import Data.HashSet qualified as S
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.Map qualified as M

newtype Intersection a = Intersection {getIntersection :: HashSet a}
    deriving stock (Eq, Generic, Show)
    deriving newtype (Hashable)

instance (Eq a, Hashable a) => Semigroup (Intersection a) where
    Intersection x <> Intersection y = Intersection (S.intersection x y)

instance From (Intersection a) (HashSet a)

instance From (HashSet a) (Intersection a)

data TodoRegistry = TodoRegistry
    { ids :: !Ids
    , idToTask :: !(IntMap Task)
    , tagToId :: !(Map Text (HashSet TaskId))
    , statusToId :: !(Map TaskStatus (HashSet TaskId))
    }
    deriving (Generic, Show)

initTodoRegistry :: TodoRegistry
initTodoRegistry = TodoRegistry initIds IM.empty M.empty M.empty

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
    deriving (Eq, Generic, Hashable, Ord, Show)

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
