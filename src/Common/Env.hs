module Common.Env
    ( module Common.Env.Config
    , module Common.Env.Runtime
    , Env (..)
    , MonadEnv
    ) where

import Common.Env.Config
import Common.Env.Runtime
import External.Prelude

data Env = Env
    { config :: !Config
    , runtime :: !Runtime
    }
    deriving (Eq, Ord, Show)

type MonadEnv m = MonadReader Env m
