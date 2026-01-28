module Effect.Error (MonadEffectError, EffectError (..)) where

import Control.Exception (IOException)
import System.IO.Error (ioeGetErrorType)

import Common
import External.Prelude

type MonadEffectError e m = MonadErrorFrom EffectError e m

data EffectError
    = GettingDataDirectoryFailed
    | GettingConfigDirectoryFailed
    | ReadFailed IOException
    | WriteFailed IOException
    | BackupFailed

instance Show EffectError where
    show GettingDataDirectoryFailed = "Failed to find data storage directory"
    show GettingConfigDirectoryFailed = "Failed to find configuration storage directory"
    show (ReadFailed e) = "Failed to load data: " <> show (ioeGetErrorType e)
    show (WriteFailed e) = "Failed to save data" <> show (ioeGetErrorType e)
    show BackupFailed = "Failed to backup data"
