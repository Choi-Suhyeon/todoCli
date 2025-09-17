module Domain.Internal 
    ( statusDueThreshold , validateName         , validateDeadline         , getTasksMatching
    , getTasksByStatus   , deleteFromMapSetKeys , insertIntoMapSetKeysWith , updateIfJust
    ) where

import Data.HashSet qualified as S
import Data.IntMap  qualified as IM
import Data.Text    qualified as T
import Data.Map     qualified as M

import Control.Monad.State.Strict (gets)
import Data.Generics.Labels       ()
import Data.Time.LocalTime        (TimeZone, utcToLocalTime)
import Data.Time.Clock            (UTCTime, NominalDiffTime, secondsToNominalDiffTime)
import Lens.Micro.Type            (Getting, ASetter)
import Data.Hashable              (Hashable)
import Data.HashSet               (HashSet)
import Lens.Micro                 ((^.), (%~))
import Data.Maybe                 (fromMaybe)
import Data.Bool                  (bool)
import Data.Text                  (Text)
import Data.Map                   (Map)

import Domain.Type.Internal 
import Domain.Error 
import Domain.Type 

statusDueThreshold :: NominalDiffTime
statusDueThreshold = secondsToNominalDiffTime $ 48 * 3600

validateName :: Text -> Either ErrCode ()
validateName n 
    | T.null n  = Left EmptyTitle 
    | otherwise = Right ()

validateDeadline :: TimeZone -> UTCTime -> UTCTime -> Either ErrCode ()
validateDeadline tz now dd
    | dd > now  = Right ()
    | otherwise = 
        let
            nowL = utcToLocalTime tz now
            ddL  = utcToLocalTime tz dd
        in
            Left $ InvalidDeadline nowL ddL

getTasksMatching ::  MonadRegistry m => (Task -> Bool) -> m (HashSet TaskId)
getTasksMatching p = gets $ IM.foldrWithKey (\k -> bool id (S.insert (TaskId k)) . p) S.empty . (^. #idToTask)

getTasksByStatus :: MonadRegistry m => TaskStatus -> m (HashSet TaskId)
getTasksByStatus s = gets $ M.findWithDefault S.empty s . (^. #statusToId)

deleteFromMapSetKeys :: (Foldable t, Ord k, Hashable a) => a -> t k -> Map k (HashSet a) -> Map k (HashSet a)
deleteFromMapSetKeys val = flip $ foldr $ M.adjust (S.delete val)

insertIntoMapSetKeysWith :: (Foldable t, Ord k, Hashable a) => (HashSet a -> HashSet a -> HashSet a) -> a -> t k -> Map k (HashSet a) -> Map k (HashSet a)
insertIntoMapSetKeysWith f val = flip $ foldr \k -> M.insertWith f k (S.singleton val)

updateIfJust :: Getting (Maybe a) s1 (Maybe a) -> ASetter s2 t a a -> s1 -> s2 -> t
updateIfJust getL overL new = overL %~ flip fromMaybe (new ^. getL)

