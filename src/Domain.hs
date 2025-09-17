module Domain
    ( module Domain.Error , module Domain.Type 
    , TaskSnapshot(..)    , EntryCreate(..)     , EntryUpdate(..)     
    , getAllTasks         , getTasksWithAllTags , getTasksByNameContaining 
    , getDoneTasks        , getUndoneTasks      , getOverdueTasks          
    , getDueTasks         , addTask             , editTask                 
    , markTask            , deleteTasks         , getTaskSnapshots
    ) where

import Data.List.NonEmpty qualified as NE
import Data.HashSet       qualified as S
import Data.IntMap        qualified as IM
import Data.Text          qualified as T
import Data.Map           qualified as M

import Control.Monad.State.Strict (MonadState(..), gets, modify')
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Error.Class  (liftEither, throwError)
import Data.Generics.Labels       ()
import Control.Monad.Reader       (ask, asks)
import Data.List.NonEmpty         (NonEmpty(..))
import Data.Time.Clock            (UTCTime, addUTCTime)
import Data.Semigroup             (Semigroup(..))
import Data.Foldable              (traverse_)
import Data.Function              (on, (&))
import Control.Monad              (when)
import GHC.Generics               (Generic)
import Data.HashSet               (HashSet)
import Lens.Micro                 ((^.), (%~), (.~))
import Data.Maybe                 (catMaybes)
import Data.Text                  (Text)

import Domain.Type.Internal 
import Domain.Internal
import Domain.Error 
import Domain.Type 
import Env        

data TaskSnapshot
    = TaskSnapshot
    { nameS     :: !Text
    , descS     :: !Text
    , tagsS     :: !(HashSet Text)
    , statusS   :: !TaskStatus
    , deadlineS :: !UTCTime
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

data EntryUpdate
    = EntryUpdate
    { name     :: !(Maybe Text)
    , desc     :: !(Maybe Text)
    , tags     :: !(Maybe (HashSet Text))
    , deadline :: !(Maybe UTCTime)
    }
  deriving (Show, Generic)

getAllTasks :: MonadRegistry m => m (HashSet TaskId)
getAllTasks = getTasksMatching (const True)

getTasksWithAllTags :: MonadRegistry m => HashSet Text -> m (HashSet TaskId)
getTasksWithAllTags tags
    | S.null tags = getAllTasks
    | otherwise   = gets \TodoRegistry{ tagToId } ->
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


