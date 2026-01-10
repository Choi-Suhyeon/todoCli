module Effect.Error (MonadEffectError, EffectError (..)) where

import Control.Exception (IOException)
import System.IO.Error (ioeGetErrorType)

import Common.Prelude

type MonadEffectError e m = (MonadError e m, From EffectError e)

data EffectError
    = GettingDataDirectoryFailed
    | ReadFailed IOException
    | WriteFailed IOException
    | BackupFailed

instance Show EffectError where
    show GettingDataDirectoryFailed = "Failed to find data storage directory"
    show (ReadFailed e) = "Failed to load data: " <> show (ioeGetErrorType e)
    show (WriteFailed e) = "Failed to save data" <> show (ioeGetErrorType e)
    show BackupFailed = "Failed to backup data"
