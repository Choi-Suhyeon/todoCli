module Common.Env (Env (..), MonadEnv) where

import Control.Monad.Reader (MonadReader)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeZone)

data Env = Env
    { now :: !UTCTime
    , tz :: !TimeZone
    }
    deriving (Eq, Ord, Show)

type MonadEnv m = MonadReader Env m
