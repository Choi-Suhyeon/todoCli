module Common.Env (Env (..), MonadEnv) where

import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeZone)

import Common.Prelude

data Env = Env
    { now :: !UTCTime
    , tz :: !TimeZone
    }
    deriving (Eq, Ord, Show)

type MonadEnv m = MonadReader Env m
