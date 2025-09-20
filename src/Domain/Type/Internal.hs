module Domain.Type.Internal 
    ( Intersection(..)
    , TodoRegistry(..) , initTodoRegistry
    , Task(..)
    , TaskStatus(..)
    , TaskId(..)       , minTaskId
    , Ids(..)          , initIds          , allocId , releaseId
    ) where


import Data.HashSet qualified as S
import Data.IntSet  qualified as IS
import Data.IntMap  qualified as IM
import Data.Map     qualified as M

import Data.Generics.Labels         ()
import Data.Time.Clock              (UTCTime)
import Data.Bifunctor               (bimap)
import Data.Hashable                (Hashable)
import GHC.Generics                 (Generic)
import Data.HashSet                 (HashSet)
import Data.IntSet                  (IntSet)
import Data.IntMap                  (IntMap)
import Data.Tuple                   (swap)
import Lens.Micro                   ((&), (%~), (.~))
import Data.Text                    (Text)
import Data.Map                     (Map)

newtype Intersection a = Intersection { getIntersection :: HashSet a }
  deriving (Show, Generic)

instance (Eq a, Hashable a) => Semigroup (Intersection a) where
    Intersection x <> Intersection y = Intersection (S.intersection x y)

data TodoRegistry
    = TodoRegistry
    { ids        :: !Ids
    , idToTask   :: !(IntMap Task)
    , tagToId    :: !(Map Text (HashSet TaskId))
    , statusToId :: !(Map TaskStatus (HashSet TaskId))
    }
  deriving (Show, Generic)

initTodoRegistry :: TodoRegistry
initTodoRegistry = TodoRegistry initIds IM.empty M.empty M.empty

data Task 
    = Task
    { name     :: !Text
    , desc     :: !Text
    , tags     :: !(HashSet Text)
    , status   :: !TaskStatus
    , deadline :: !UTCTime
    }
  deriving (Show, Generic)

data TaskStatus
    = Done
    | Undone
  deriving (Show, Generic, Eq, Ord, Hashable)

newtype TaskId = TaskId { unTaskId :: Int }
  deriving stock   (Show, Generic, Eq, Ord)
  deriving newtype (Hashable)

minTaskId :: TaskId
minTaskId = TaskId 1

data Ids
    = Ids
    { next     :: !TaskId
    , released :: !IntSet
    }
  deriving (Show, Generic)

initIds :: Ids
initIds = Ids minTaskId IS.empty

allocId :: Ids -> (Ids, TaskId)
allocId ids
    | IS.null ids.released = (ids & #next . #unTaskId %~ succ, ids.next)
    | otherwise            = bimap (($ ids) . (#released .~)) TaskId . swap . IS.deleteFindMin $ ids.released

releaseId :: TaskId -> Ids -> Ids
releaseId tid ids 
    | isValidId = interim
    | otherwise = ids
  where
    isValidId     = and @[] $ [(>= minTaskId), (< ids.next)] <*> [tid]
    isMaxId       = (tid & #unTaskId %~ succ) == ids.next
    isDoubleFreed = IS.member tid.unTaskId ids.released
    interim
        | isDoubleFreed = ids
        | isMaxId       = ids & #next . #unTaskId %~ pred
        | otherwise     = ids & #released %~ (IS.insert tid.unTaskId)
            

