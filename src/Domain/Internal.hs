module Domain.Internal (Task(..), TaskStatus(..), TaskId(..), minTaskId, Ids, initIds, allocId, releaseId) where

import Data.Map qualified as M
import Data.IntSet  qualified as IS
import Data.Text    qualified as T

import Data.Time.Clock (UTCTime)
import Data.Bifunctor  (bimap)
import Data.Function   (on)
import Control.Arrow   ((&&&))
import Control.Monad   (guard)
import Data.Hashable   (Hashable)
import GHC.Generics    (Generic)
import Data.HashSet    (HashSet)
import Data.Map    (Map)
import Data.IntMap     (IntMap)
import Data.IntSet     (IntSet)
import Data.Tuple      (swap)
import Data.Bool       (bool)
import Data.Text       (Text)

import Control.Monad.State.Strict (MonadState(..))
import Lens.Micro (Lens'(..), (&), (^.), (%~), (.~))
import Data.Generics.Product.Fields ()
import Data.Generics.Product (field)
import Data.Generics.Labels ()

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
  deriving 
    (Hashable, Generic, Show, Ord, Eq)

newtype TaskId = TaskId { unTaskId :: Int }
  deriving (Show, Generic, Hashable, Eq, Ord)

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
    isValidId = and @[] $ [(>= minTaskId), (< ids.next)] <*> [tid]
    isMaxId   = (tid & #unTaskId %~ succ) == ids.next
    isDoubleFreed = IS.member tid.unTaskId ids.released
    interim
        | isDoubleFreed = ids
        | isMaxId       = ids & #next . #unTaskId %~ pred
        | otherwise     = ids & #released %~ (IS.insert tid.unTaskId)
            
