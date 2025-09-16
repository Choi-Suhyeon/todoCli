module Domain
    ( TaskSnapshot(..)         , EntryUpdate(..) , EntryCreate(..)  , TodoRegistry        , MonadRegistry
    , initTodoRegistry         , getAllTasks     , getTasksMatching , getTasksWithAllTags , getTasksByStatus
    , getTasksByNameContaining , getDoneTasks    , getUndoneTasks   , getOverdueTasks     , getDueTasks
    , addTask                  , editTask        , markTask         , deleteTasks         , getTaskSnapshots
    )
  where

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
import Control.Monad.Reader         (ask, asks)
import Data.Time.LocalTime          (utcToLocalTime, getCurrentTimeZone)
import Data.List.NonEmpty           (NonEmpty(..))
import Lens.Micro.Type              (Getting, ASetter)
import Data.Foldable                (for_)
import Data.Function                (on)
import Data.Hashable                (Hashable)
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
import Env          (Env(..), MonadEnv)

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

data EntryUpdate
    = EntryUpdate
    { name     :: !(Maybe Text)
    , desc     :: !(Maybe Text)
    , tags     :: !(Maybe (HashSet Text))
    , deadline :: !(Maybe UTCTime)
    }
  deriving (Show, Generic)

data EntryCreate
    = EntryCreate
    { name     :: !Text
    , desc     :: !Text
    , tags     :: !(HashSet Text)
    , deadline :: !UTCTime
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

getAllTasks :: MonadRegistry m => m (HashSet TaskId)
getAllTasks = gets $ getTaskIdsMatching (const True)

getTasksMatching :: HasField' "idToTask" t (IntMap a) => (a -> Bool) -> t -> HashSet TaskId
getTasksMatching p = IM.foldrWithKey (\k _ -> S.insert (TaskId k)) S.empty . IM.filter p . (^. #idToTask)

getTasksWithAllTags :: MonadRegistry m => HashSet Text -> m (HashSet TaskId)
getTasksWithAllTags tags
    | S.null tags = getAllTasks
    | otherwise   = gets $ \TodoRegistry{ tagToId } -> foldr (\t -> maybe id S.intersection (M.lookup t tagToId)) S.empty tags

getTasksByStatus :: (HasField' "statusToId" t (Map k (HashSet a)), Ord k) => k -> t -> HashSet a
getTasksByStatus s = fromMaybe S.empty . M.lookup s . (^. #statusToId)

getTasksByNameContaining :: MonadRegistry m => Text -> m (HashSet TaskId)
getTasksByNameContaining subs = gets $ getTaskIdsMatching ((subs `isInfixOfI`) . (^. #name))
  where
    isInfixOfI :: Text -> Text -> Bool
    isInfixOfI = T.isInfixOf `on` T.toCaseFold

getDoneTasks :: MonadRegistry m => m (HashSet TaskId)
getDoneTasks = gets $ getTaskIdsByStatus Done

getUndoneTasks :: MonadRegistry m => m (HashSet TaskId)
getUndoneTasks = gets $ getTaskIdsByStatus Undone

getOverdueTasks :: (MonadRegistry m, MonadEnv m) => m (HashSet TaskId)
getOverdueTasks = do
    now                      <- asks (^. #now)
    undoneIds                <- getUndoneTasks
    TodoRegistry{ idToTask } <- get

    let 
        isOverdue (TaskId id) = maybe False ((<= now) . (^. #deadline)) $ idToTask IM.!? id

    pure $ S.filter isOverdue undoneIds

getDueTasks :: (MonadRegistry m, MonadEnv m) => m (HashSet TaskId)
getDueTasks = do
    now                      <- asks (^. #now)
    undoneIds                <- getUndoneTasks
    TodoRegistry{ idToTask } <- get

    let 
        isDue'            = liftA2 (&&) (> now) (<= addUTCTime statusDueThreshold now)
        isDue (TaskId id) = maybe False (isDue' . (^. #deadline)) $ idToTask IM.!? id

    pure $ S.filter isDue undoneIds

addTask :: (MonadRegistry m, MonadEnv m) => EntryCreate -> m (Result ())
addTask e = pure =<< runExceptT do
    Env{ .. } <- lift ask

    let 
        isValidName n
            | T.null n  = Left  EmptyTitle
            | otherwise = Right ()

        isValidDeadline dd
            | dd <= now = Left $ InvalidDeadline nowL ddL
            | otherwise = Right ()
              where
                nowL = utcToLocalTime tz now
                ddL  = utcToLocalTime tz dd

    liftEither $ isValidName e.name
    liftEither $ isValidDeadline e.deadline

    lift do
        todoRegi@TodoRegistry{ .. } <- get

        let 
            (ids', tid) = allocId ids

        put
            $ todoRegi
            & #ids        .~ ids'            
            & #statusToId %~ insertToMapSetKeysWith (<>) tid (Undone :| [])
            & #tagToId    %~ insertToMapSetKeysWith (<>) tid e.tags
            & #idToTask   %~ IM.insert tid.unTaskId 
                Task
                    { name     = e.name
                    , desc     = e.desc
                    , tags     = e.tags
                    , deadline = e.deadline
                    , status   = Undone
                    }
        pure ()

editTask :: (MonadRegistry m, MonadEnv m) => EntryUpdate -> TaskId -> m (Result ())
editTask e tid = pure =<< runExceptT do
    Env{ .. }                   <- lift ask
    todoRegi@TodoRegistry{ .. } <- lift get

    let 
        isValidName n
            | T.null n  = Left  EmptyTitle
            | otherwise = Right ()

        isValidDeadline dd
            | dd <= now = Left $ InvalidDeadline nowL ddL
            | otherwise = Right ()
              where
                nowL = utcToLocalTime tz now
                ddL  = utcToLocalTime tz dd

    liftEither . maybe (pure ()) isValidName     $ e.name
    liftEither . maybe (pure ()) isValidDeadline $ e.deadline

    oldTask <- liftEither . maybe (Left TaskNotFound) Right $ idToTask IM.!? tid.unTaskId

    lift do
        let 
            newTask 
                = oldTask 
                & updateIfJust #name     #name     e 
                . updateIfJust #desc     #desc     e 
                . updateIfJust #tags     #tags     e 
                . updateIfJust #deadline #deadline e

        put 
            $ todoRegi
            & #idToTask %~ IM.insert tid.unTaskId newTask
            & #tagToId  %~ maybe id (\ts -> insertToMapSetKeysWith (<>) tid ts . deleteFromMapSetKeys tid oldTask.tags) e.tags
        pure ()

markTask :: MonadRegistry m => TaskId -> TaskStatus -> m ()
markTask tid s = do
    reg@TodoRegistry{..} <- get

    for_ (IM.lookup tid.unTaskId idToTask) \task -> do
        let 
            updatedTask   = task & #status .~ s 
            newIdToTask   = IM.insert tid.unTaskId updatedTask idToTask
            newStatusToId = M.insertWith S.union s (S.singleton tid) $ M.map (S.delete tid) statusToId

        put reg { idToTask = newIdToTask, statusToId = newStatusToId }

deleteTasks :: MonadRegistry m => HashSet TaskId -> m ()
deleteTasks tids = get >>= \todoRegi -> put (foldr go todoRegi tids) >> pure ()
  where
    go tid original@TodoRegistry{ .. }
        | Just (Task { tags, status }) <- idToTask IM.!? tid.unTaskId
            = original 
            & #tagToId    %~ deleteFromMapSetKeys tid tags
            & #statusToId %~ deleteFromMapSetKeys tid (status :| []) 
            & #idToTask   %~ IM.delete tid.unTaskId 
            & #ids        %~ releaseId tid
        | otherwise 
            = original

getTaskSnapshots :: MonadRegistry m => HashSet TaskId -> m [TaskSnapshot]
getTaskSnapshots tids = do
    TodoRegistry{ idToTask } <- get
    pure 
        $ catMaybes 
        . map 
            ( (fromTaskToSnapshot <$>)
            . (idToTask IM.!?) 
            . (^. #unTaskId)
            ) 
        . S.toList
        $ tids
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

statusDueThreshold :: NominalDiffTime
statusDueThreshold = secondsToNominalDiffTime $ 48 * 3600

deleteFromMapSetKeys :: (Foldable t, Ord k, Hashable a) => a -> t k -> Map k (HashSet a) -> Map k (HashSet a)
deleteFromMapSetKeys val = flip $ foldr $ M.adjust (S.delete val)

insertToMapSetKeysWith :: (Foldable t, Ord k, Hashable a) => (HashSet a -> HashSet a -> HashSet a) -> a -> t k -> Map k (HashSet a) -> Map k (HashSet a)
insertToMapSetKeysWith f val = flip $ foldr \k -> M.insertWith f k (S.singleton val)

updateIfJust :: Getting (Maybe a) s1 (Maybe a) -> ASetter s2 t a a -> s1 -> s2 -> t
updateIfJust getL overL new = overL %~ flip fromMaybe (new ^. getL)

