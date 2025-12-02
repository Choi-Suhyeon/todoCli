module Domain.Internal
    ( statusDueThreshold
    , validateDeadline
    , getTasksMatching
    , getTasksByStatus
    , deleteFromSetsAtKeys
    , insertIntoSetsAtKeysWith
    , updateIfJust
    ) where

import Control.Monad.State.Strict (gets)
import Data.Generics.Labels ()
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Time.Clock
    ( NominalDiffTime
    , UTCTime
    , secondsToNominalDiffTime
    )
import Data.Time.LocalTime (TimeZone, utcToLocalTime)
import Lens.Micro ((%~), (^.))
import Lens.Micro.Type (ASetter, Getting)
import Witch

import Data.HashSet qualified as S
import Data.IntMap qualified as IM
import Data.Map qualified as M

import Domain.Error
import Domain.Type
import Domain.Type.Internal

statusDueThreshold :: NominalDiffTime
statusDueThreshold = secondsToNominalDiffTime $ 48 * 3600

validateDeadline :: TimeZone -> UTCTime -> UTCTime -> Either DomainError ()
validateDeadline tz now dd
    | dd > now = Right ()
    | otherwise =
        let
            nowL = utcToLocalTime tz now
            ddL = utcToLocalTime tz dd
         in
            Left $ InvalidDeadline nowL ddL

getTasksMatching :: (MonadRegistry m) => (Task -> Bool) -> m (HashSet TaskId)
getTasksMatching p = gets $ IM.foldrWithKey insertKeyWhen S.empty . (^. #idToTask)
  where
    insertKeyWhen :: Int -> Task -> HashSet TaskId -> HashSet TaskId
    insertKeyWhen key task set
        | p task = S.insert (into key) set
        | otherwise = set

getTasksByStatus :: (MonadRegistry m) => TaskStatus -> m (HashSet TaskId)
getTasksByStatus = gets . (. (^. #statusToId)) . M.findWithDefault S.empty

deleteFromSetsAtKeys
    :: (Foldable t, Hashable a, Ord k) => a -> t k -> Map k (HashSet a) -> Map k (HashSet a)
deleteFromSetsAtKeys = flip . foldr . M.adjust . S.delete

insertIntoSetsAtKeysWith
    :: (Foldable t, Hashable a, Ord k)
    => (HashSet a -> HashSet a -> HashSet a)
    -> a
    -> t k
    -> Map k (HashSet a)
    -> Map k (HashSet a)
insertIntoSetsAtKeysWith f val ks m = foldr (\k -> M.insertWith f k (S.singleton val)) m ks

updateIfJust :: Getting (Maybe a) s1 (Maybe a) -> ASetter s2 t a a -> s1 -> s2 -> t
updateIfJust getL overL new = overL %~ (`fromMaybe` (new ^. getL))
