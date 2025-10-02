module Domain.Internal
    ( statusDueThreshold
    , validateDeadline
    , getTasksMatching
    , getTasksByStatus
    , deleteFromMapSetKeys
    , insertIntoMapSetKeysWith
    , updateIfJust
    ) where

import Control.Monad.State.Strict (gets)
import Data.Bool (bool)
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
getTasksMatching p =
    gets
        $ IM.foldrWithKey (\k -> bool id (S.insert (TaskId k)) . p) S.empty
            . (^. #idToTask)

getTasksByStatus :: (MonadRegistry m) => TaskStatus -> m (HashSet TaskId)
getTasksByStatus s = gets $ M.findWithDefault S.empty s . (^. #statusToId)

deleteFromMapSetKeys
    :: (Foldable t, Hashable a, Ord k)
    => a -> t k -> Map k (HashSet a) -> Map k (HashSet a)
deleteFromMapSetKeys val = flip $ foldr $ M.adjust (S.delete val)

insertIntoMapSetKeysWith
    :: (Foldable t, Hashable a, Ord k)
    => (HashSet a -> HashSet a -> HashSet a)
    -> a
    -> t k
    -> Map k (HashSet a)
    -> Map k (HashSet a)
insertIntoMapSetKeysWith f val = flip $ foldr \k -> M.insertWith f k (S.singleton val)

updateIfJust
    :: Getting (Maybe a) s1 (Maybe a) -> ASetter s2 t a a -> s1 -> s2 -> t
updateIfJust getL overL new = overL %~ flip fromMaybe (new ^. getL)
