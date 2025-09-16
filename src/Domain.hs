module Domain
    ( TaskSnapshot(..)         , EntryUpdate(..) , EntryCreate(..)     
    , TodoRegistry             , MonadRegistry
    , initTodoRegistry         , getAllTasks     , getTasksWithAllTags 
    , getTasksByNameContaining , getDoneTasks    , getUndoneTasks  
    , getOverdueTasks          , getDueTasks     , addTask                  
    , editTask                 , markTask        , deleteTasks         
    , getTaskSnapshots
    ) where

import Data.List.NonEmpty qualified as NE
import Data.HashSet       qualified as S
import Data.IntMap        qualified as IM
import Data.Text          qualified as T
import Data.Map           qualified as M

import Data.Generics.Product.Fields ()
import Control.Monad.State.Strict   (MonadState(..), gets, modify')
import Control.Monad.Trans.Except   (runExceptT)
import Control.Monad.Error.Class    (liftEither, throwError)
import Data.Generics.Product        ()
import Data.Generics.Labels         ()
import Control.Monad.Reader         (ask, asks)
import Data.Time.LocalTime          (TimeZone, utcToLocalTime)
import Data.List.NonEmpty           (NonEmpty(..))
import Data.Time.Clock 
    ( UTCTime    , NominalDiffTime
    , addUTCTime , secondsToNominalDiffTime
    )
import Lens.Micro.Type              (Getting, ASetter)
import Data.Semigroup               (Semigroup(..))
import Data.Foldable                (traverse_)
import Data.Hashable                (Hashable)
import Data.Function                (on, (&))
import Control.Monad                (when)
import GHC.Generics                 (Generic)
import Data.HashSet                 (HashSet)
import Data.IntMap                  (IntMap)
import Lens.Micro                   ((^.), (%~), (.~))
import Data.Maybe                   (fromMaybe, catMaybes)
import Data.Bool                    (bool)
import Data.Text                    (Text)
import Data.Map                     (Map)

import Domain.Internal
    ( Task(..)  , TaskStatus(..) , TaskId(..) , Ids
    , allocId   , releaseId      , initIds 
    )
import Domain.Error     (ErrCode(..), Result)
import Env              (Env(..), MonadEnv)

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

newtype Intersection a = Intersection { getIntersection :: HashSet a }
  deriving (Show, Generic)

instance (Eq a, Hashable a) => Semigroup (Intersection a) where
    Intersection x <> Intersection y = Intersection (S.intersection x y)

initTodoRegistry :: TodoRegistry
initTodoRegistry = TodoRegistry initIds IM.empty M.empty M.empty

getAllTasks :: MonadRegistry m => m (HashSet TaskId)
getAllTasks = getTasksMatching (const True)

getTasksWithAllTags :: MonadRegistry m => HashSet Text -> m (HashSet TaskId)
getTasksWithAllTags tags
    | S.null tags = getAllTasks
    | otherwise   = gets $ \TodoRegistry{ tagToId } ->
        tags
            & S.toList
            & map (Intersection . flip (M.findWithDefault S.empty) tagToId)
            & NE.nonEmpty
            & maybe S.empty ((^. #getIntersection) . sconcat)
   
getTasksByNameContaining :: MonadRegistry m => Text -> m (HashSet TaskId)
getTasksByNameContaining subs = getTasksMatching ((subs `isInfixOfI`) . (^. #name))
  where
    isInfixOfI :: Text -> Text -> Bool
    isInfixOfI = T.isInfixOf `on` T.toCaseFold

getDoneTasks :: MonadRegistry m => m (HashSet TaskId)
getDoneTasks = getTasksByStatus Done

getUndoneTasks :: MonadRegistry m => m (HashSet TaskId)
getUndoneTasks = getTasksByStatus Undone

getOverdueTasks :: (MonadRegistry m, MonadEnv m) => m (HashSet TaskId)
getOverdueTasks = do
    now                      <- asks (^. #now)
    undoneIds                <- getUndoneTasks
    TodoRegistry{ idToTask } <- get

    let 
        isOverdue (TaskId tid) = maybe False ((<= now) . (^. #deadline)) $ idToTask IM.!? tid

    pure $ S.filter isOverdue undoneIds

getDueTasks :: (MonadRegistry m, MonadEnv m) => m (HashSet TaskId)
getDueTasks = do
    now                      <- asks (^. #now)
    undoneIds                <- getUndoneTasks
    TodoRegistry{ idToTask } <- get

    let 
        isDue'             = liftA2 (&&) (> now) (<= addUTCTime statusDueThreshold now)
        isDue (TaskId tid) = maybe False (isDue' . (^. #deadline)) $ idToTask IM.!? tid

    pure $ S.filter isDue undoneIds

addTask :: (MonadRegistry m, MonadEnv m) => EntryCreate -> m (Result ())
addTask e = runExceptT do
    Env{ .. } <- ask

    traverse_ @[] liftEither
        [ validateName            e.name
        , validateDeadline tz now e.deadline
        ]

    (ids', tid) <- gets (allocId . (^. #ids))

    modify'
        $ (#ids        .~ ids')
        . (#statusToId %~ insertIntoMapSetKeysWith (<>) tid (Undone :| []))
        . (#tagToId    %~ insertIntoMapSetKeysWith (<>) tid e.tags)
        . (#idToTask   %~ IM.insert tid.unTaskId newTask)
  where
    newTask = Task
        { name     = e.name
        , desc     = e.desc
        , tags     = e.tags
        , deadline = e.deadline
        , status   = Undone
        }

editTask :: (MonadRegistry m, MonadEnv m) => EntryUpdate -> TaskId -> m (Result ())
editTask e tid = runExceptT do
    Env{ .. } <- ask

    traverse_ @[] liftEither
        [ maybe (pure ()) validateName              e.name
        , maybe (pure ()) (validateDeadline tz now) e.deadline
        ]

    gets ((IM.!? tid.unTaskId) . (^. #idToTask)) >>= \case
        Nothing  -> throwError TaskNotFound
        Just old -> modify' 
            $ (#idToTask %~ IM.insert tid.unTaskId newTask)
            . (#tagToId  %~ maybe id updateTags e.tags)
          where
            newTask 
                = old 
                & updateIfJust #name     #name     e
                . updateIfJust #desc     #desc     e
                . updateIfJust #tags     #tags     e
                . updateIfJust #deadline #deadline e

            updateTags ts 
                = insertIntoMapSetKeysWith (<>) tid ts 
                . deleteFromMapSetKeys tid old.tags

markTask :: MonadRegistry m => TaskId -> TaskStatus -> m ()
markTask tid s = do
    exists <- gets $ IM.member tid.unTaskId . (^. #idToTask)

    when exists . modify' 
        $ (#idToTask %~ IM.adjust (#status .~ s) tid.unTaskId)
        . (#statusToId %~ M.insertWith S.union s (S.singleton tid) . M.map (S.delete tid))

deleteTasks :: MonadRegistry m => HashSet TaskId -> m ()
deleteTasks = modify' . flip (foldl' go)
  where
    go :: TodoRegistry -> TaskId -> TodoRegistry
    go original@TodoRegistry{ .. } tid = go' original
      where
        go' = idToTask IM.!? tid.unTaskId & maybe id \Task{ tags, status } x -> 
            x
                & #tagToId    %~ deleteFromMapSetKeys tid tags
                & #statusToId %~ deleteFromMapSetKeys tid (status :| []) 
                & #idToTask   %~ IM.delete tid.unTaskId 
                & #ids        %~ releaseId tid

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

