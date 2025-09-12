module Domain ( ) where

import Data.HashSet qualified as S
import Data.IntMap  qualified as IM
import Data.IntSet  qualified as IS
import Data.Text    qualified as T
import Data.Map     qualified as M

import Data.Generics.Product.Fields (HasField')
import Control.Monad.State.Strict   (MonadState(..), lift, gets)
import Control.Monad.Trans.Except   (runExceptT)
import Control.Monad.Error.Class    (liftEither)
import Control.Monad.IO.Class       (MonadIO(..))
import Data.Generics.Product        (field)
import Data.Generics.Labels         ()
import Data.Time.LocalTime          (utcToLocalTime, getCurrentTimeZone)
import Data.Foldable                (for_)
import Data.Function                (on)
import GHC.Generics                 (Generic)
import Data.HashSet                 (HashSet)
import Data.IntMap                  (IntMap)
import Data.IntSet                  (IntSet)
import Lens.Micro                   (Lens'(..), (<&>), (&), (^.), (%~), (.~))
import Data.Maybe                   (fromMaybe, catMaybes, maybe)
import Data.Text                    (Text)
import Data.Map                     (Map)

import Data.Time.Clock              
    ( UTCTime    , secondsToNominalDiffTime , diffUTCTime
    , addUTCTime , NominalDiffTime          , getCurrentTime
    )

import Domain.Error (ErrCode(..), Result)
import Env (Env(..), MonadEnv)

import Domain.Internal 
    ( Task(..)  , TaskStatus(..) , TaskId(..) , Ids
    , minTaskId , releaseId      , allocId    , initIds 
    )

data TaskSnapshot
    = TaskSnapshot
    { nameS     :: !Text
    , descS     :: !Text
    , tagsS     :: !(HashSet Text)
    , statusS   :: !TaskStatus
    , deadlineS :: !UTCTime
    }
  deriving (Show, Generic)

data TodoRegistry
    = TodoRegistry
    { ids        :: !Ids
    , idToTask   :: !(IntMap Task)
    , tagToId    :: !(Map Text (HashSet TaskId))
    , statusToId :: !(Map TaskStatus (HashSet TaskId))
    }
  deriving (Show, Generic)

type MonadRegistry m = MonadState TodoRegistry m

initTodoRegistry :: TodoRegistry
initTodoRegistry = TodoRegistry initIds IM.empty M.empty M.empty

getTaskIdsMatching :: HasField' "idToTask" t (IntMap a) => (a -> Bool) -> t -> HashSet TaskId
getTaskIdsMatching p = IM.foldrWithKey (\k _ -> S.insert (TaskId k)) S.empty . IM.filter p . (^. #idToTask)

getTaskIdsByStatus :: (HasField' "statusToId" t (Map k (HashSet a)), Ord k) => k -> t -> HashSet a
getTaskIdsByStatus s = fromMaybe S.empty . M.lookup s . (^. #statusToId)

statusDueThreshold :: NominalDiffTime
statusDueThreshold = secondsToNominalDiffTime $ 48 * 3600

getAllTasks :: MonadRegistry m => m (HashSet TaskId)
getAllTasks = gets $ getTaskIdsMatching (const True)

getTasksByNameContaining :: MonadRegistry m => Text -> m (HashSet TaskId)
getTasksByNameContaining subs = gets $ getTaskIdsMatching ((subs `isInfixOfI`) . (^. #name))
  where
    isInfixOfI :: Text -> Text -> Bool
    isInfixOfI = T.isInfixOf `on` T.toCaseFold

getDoneTasks :: MonadRegistry m => m (HashSet TaskId)
getDoneTasks = gets $ getTaskIdsByStatus Done

getUndoneTasks :: MonadRegistry m => m (HashSet TaskId)
getUndoneTasks = gets $ getTaskIdsByStatus Undone

getTasksWithAllTags :: MonadRegistry m => HashSet Text -> m (HashSet TaskId)
getTasksWithAllTags tags
    | S.null tags = getAllTasks
    | otherwise   = gets $ \TodoRegistry{ tagToId } -> foldr (\t -> maybe id S.intersection (M.lookup t tagToId)) S.empty tags

-- todo : write functions: getDueTasks and getOverdueTasks / write a function for editing an existing task

getDueTasks :: (MonadRegistry m, MonadEnv m) => m (HashSet TaskId)
getDueTasks = undefined

getOverdueTask :: (MonadRegistry m, MonadEnv m) => m (HashSet TaskId)
getOverdueTask = undefined     

{-
isDueTask :: UTCTime -> Task -> Bool
isDueTask now = liftA2 (&&) isUndoneTask ((>= 0) . (`diffUTCTime` now) . (addUTCTime statusDueThreshold) . (^. #deadline))

isOverdueTask :: UTCTime -> Task -> Bool
isOverdueTask now = liftA2 (&&) isUndoneTask ((>= 0) . (`diffUTCTime` now) . (^. #deadline))

isUndoneTask :: Task -> Bool
isUndoneTask = (== Undone) . (^. #status)
-}

addTask :: (MonadRegistry m, MonadIO m) => Text -> Text -> HashSet Text -> UTCTime -> m (Result ())
addTask nm dsc ts dd = pure =<< runExceptT do
    liftEither $ isValidName nm
    liftEither =<< liftIO (isValidDeadline dd)

    lift do
        TodoRegistry { .. } <- get

        let (ids', tid) = allocId ids
        let tidSet      = S.singleton tid
        let newTask     = Task { name = nm, desc = dsc, tags = ts, deadline = dd, status = Undone }

        put TodoRegistry 
            { ids        = ids'
            , idToTask   = IM.insert tid.unTaskId newTask idToTask
            , statusToId = M.insertWith (<>) Undone tidSet statusToId
            , tagToId    = foldr (\tag -> M.insertWith (<>) tag tidSet) tagToId ts
            }
        pure ()
  where
    isValidName :: Text -> Result ()
    isValidName n
        | T.null n  = Left  EmptyTitle
        | otherwise = Right ()

    isValidDeadline :: UTCTime -> IO (Result ())
    isValidDeadline dd = do
        cur <- getCurrentTime        

        case diffUTCTime dd cur `compare` 0 of 
            GT -> pure $ Right () 
            _  -> do
                tz <- getCurrentTimeZone

                let curL = utcToLocalTime tz cur
                let ddL  = utcToLocalTime tz dd

                pure . Left $ InvalidDeadline curL ddL

getTaskSnapshots :: MonadRegistry m => HashSet TaskId -> m [TaskSnapshot]
getTaskSnapshots tids = get >>= \TodoRegistry{ idToTask } -> pure . catMaybes . map (fmap fromTaskToSnapshot . (idToTask IM.!?) . (^. #unTaskId)) . S.toList $ tids
  where
    fromTaskToSnapshot :: Task -> TaskSnapshot
    fromTaskToSnapshot (Task { .. }) 
        = TaskSnapshot 
        { nameS     = name
        , descS     = desc
        , tagsS     = tags
        , statusS   = status
        , deadlineS = deadline
        }

deleteTasks :: MonadRegistry m => HashSet TaskId -> m ()
deleteTasks tids = get >>= \todoReg -> put (foldr go todoReg tids) >> pure ()
  where
    go tid original@TodoRegistry{ .. }
        | Just (Task { tags, status }) <- idToTask IM.!? tid.unTaskId
            = TodoRegistry
            { tagToId    = delIdfromTags tid tags tagToId
            , statusToId = delIdfromStatuses tid status statusToId
            , idToTask   = IM.delete tid.unTaskId idToTask 
            , ids        = releaseId tid ids 
            }
        | otherwise 
            = original

    delIdfromTags :: TaskId -> HashSet Text -> Map Text (HashSet TaskId) -> Map Text (HashSet TaskId)
    delIdfromTags tid tags = flip (foldr $ M.adjust (S.delete tid)) tags

    delIdfromStatuses :: TaskId -> TaskStatus -> Map TaskStatus (HashSet TaskId) -> Map TaskStatus (HashSet TaskId)
    delIdfromStatuses tid status = M.adjust (S.delete tid) status

mark :: MonadRegistry m => TaskId -> TaskStatus -> m ()
mark tid s = do
    reg@TodoRegistry{..} <- get

    for_ (IM.lookup tid.unTaskId idToTask) \task -> do
        let updatedTask   = task & #status .~ s 
        let newIdToTask   = IM.insert tid.unTaskId updatedTask idToTask
        let newStatusToId = M.insertWith S.union s (S.singleton tid) $ M.map (S.delete tid) statusToId

        put reg { idToTask = newIdToTask, statusToId = newStatusToId }


