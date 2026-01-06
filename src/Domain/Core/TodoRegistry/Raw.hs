module Domain.Core.TodoRegistry.Raw (TodoRegistry (..)) where

import Data.HashSet (HashSet)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Serialize (Serialize (..))
import Data.Text (Text)

import Common.Prelude hiding (get, put)
import Domain.Core.Task.Internal
import Domain.Core.TaskId.Internal

data TodoRegistry = TodoRegistry
    { ids :: !Ids
    , idToTask :: !(IntMap Task)
    , tagToId :: !(Map Text (HashSet TaskId))
    , statusToId :: !(Map TaskStatus (HashSet TaskId))
    }
    deriving (Show)

instance Serialize TodoRegistry where
    put TodoRegistry{..} = do
        put ids
        put idToTask
        put tagToId
        put statusToId

    get = TodoRegistry <$> get <*> get <*> get <*> get
