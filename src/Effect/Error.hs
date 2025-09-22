module Effect.Error (MonadEffectError, EffectError(..)) where

import Control.Monad.Except (MonadError(..))

import Common (FromErr)

type MonadEffectError e m = (MonadError e m, FromErr EffectError e)

data EffectError
    = GeneralIOE String
    | GettingDataDirectoryFailed
    | ReadFailed
    | WriteFailed

instance Show EffectError where
    show (GeneralIOE s) = s
    show GettingDataDirectoryFailed = "Failed to find data storage directory"
    show ReadFailed = "Failed to load data"
    show WriteFailed = "Failed to save data"

