module Domain ( ) where

import Data.Map qualified as M
import Data.HashSet qualified as S
import Data.IntMap  qualified as IM
import Data.IntSet  qualified as IS
import Data.Text    qualified as T

import Data.Map    (Map)
import Data.HashSet    (HashSet)
import Data.IntMap     (IntMap)
import Data.IntSet     (IntSet)
import Data.Text       (Text)
import Data.Function (on)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Strict (MonadState(..))
import Lens.Micro (Lens'(..), (&), (^.), (%~), (.~))
import Data.Generics.Product.Fields ()
import Data.Generics.Product (field)
import Data.Generics.Labels ()
import Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)
import Data.Time.LocalTime (utcToLocalTime, getCurrentTimeZone)
import GHC.Generics    (Generic)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Error.Class (liftEither)

import Domain.Error (ErrCode(..), Result)
import Domain.Internal 
    ( Task(..)  , TaskStatus(..) , TaskId(..) , Ids
    , minTaskId , releaseId      , allocId    , initIds 
    )

data TodoRegistry
    = TodoRegistry
    { ids        :: !Ids
    , tasks      :: !(IntMap Task)
    , tagToId    :: !(Map Text IntSet)
    , statusToId :: !(Map TaskStatus IntSet)
    }
  deriving (Show, Generic)

initTodoRegistry :: TodoRegistry
initTodoRegistry = TodoRegistry initIds IM.empty M.empty M.empty

-- todo : error maybe be occurred. consider this is sample
addTask :: (MonadState TodoRegistry m, MonadIO m) => Text -> Text -> HashSet Text -> UTCTime -> m (Result ())
addTask nm dsc ts dd = do
    r <- runExceptT do
        liftEither $ isValidName nm
        liftEither =<< liftIO (isValidDeadline dd)

        lift do
            TodoRegistry { .. } <- get

            let (ids', tid) = allocId ids
            let rawTid = tid.unTaskId
            let rawTidSet = IS.singleton rawTid
            let newTask = Task { name = nm, desc = dsc, tags = ts, deadline = dd, status = Undone }
            let tagToId' = foldr (\tag -> M.insertWith (<>) tag rawTidSet) tagToId ts
            let statusToId' = M.insertWith (<>) Undone rawTidSet statusToId
            let tasks' = IM.insert rawTid newTask tasks

            put TodoRegistry { ids = ids', tagToId = tagToId', statusToId = statusToId', tasks = tasks' }
            pure $ Right ()
    pure r
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

