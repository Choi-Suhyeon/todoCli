module Effect.Error (MonadEffectError, EffectError (..)) where

import Common.Prelude

type MonadEffectError e m = (MonadError e m, From EffectError e)

data EffectError
    = GettingDataDirectoryFailed
    | ReadFailed
    | WriteFailed

instance Show EffectError where
    show GettingDataDirectoryFailed = "Failed to find data storage directory"
    show ReadFailed = "Failed to load data"
    show WriteFailed = "Failed to save data"
