module Common.Log (MonadLog, Log, LogType (..), logMsg) where

import Data.Sequence (Seq)
import Data.Text (Text)

import Data.List qualified as L

import External.Prelude

type MonadLog m = MonadWriter Log m

type Log = Seq Text

data LogType
    = LogWarning
    | LogInfo

instance Show LogType where
    show LogWarning = "[W]"
    show LogInfo = "[I]"

logMsg :: (MonadLog m) => LogType -> Text -> m ()
logMsg t l = tell . toSeq $ toTextType t <> l <> "\n"
  where
    toSeq :: Text -> Seq Text
    toSeq = into . L.singleton

    toTextType :: LogType -> Text
    toTextType = (<> " ") . into . show
