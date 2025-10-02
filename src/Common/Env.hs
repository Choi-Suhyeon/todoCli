module Common.Env (Env (..), MonadEnv) where

import Control.Monad.Reader (MonadReader)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeZone)

import Common.Optics

data Env
    = Env
    { now :: !UTCTime
    , tz :: !TimeZone
    }
    deriving (Eq, Generic, Ord, Show)

type MonadEnv m = MonadReader Env m
