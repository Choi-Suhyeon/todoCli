module Domain.Core.TodoRegistry.Raw (TodoRegistry (..)) where

import Data.HashSet (HashSet)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Serialize (Serialize (..), getWord16le, putWord16le)
import Data.Text (Text)
import Data.Word (Word16)

import Common.Prelude hiding (get, put)
import Domain.Core.Task.Internal
import Domain.Core.TaskId.Internal

latestVersion :: Word16
latestVersion = 1

data TodoRegistry = TodoRegistry
    { ids :: !Ids
    , idToTask :: !(IntMap Task)
    , tagToId :: !(Map Text (HashSet TaskId))
    , statusToId :: !(Map TaskStatus (HashSet TaskId))
    }
    deriving (Show)

instance Serialize TodoRegistry where
    put TodoRegistry{..} = do
        putWord16le latestVersion
        put ids
        put idToTask
        put tagToId
        put statusToId

    get =
        getWord16le >>= \case
            1 -> TodoRegistry <$> get <*> get <*> get <*> get
            n -> fail $ "unknown version: " <> show n
