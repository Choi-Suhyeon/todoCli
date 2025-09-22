module Common.Env (Env(..), MonadEnv) where

import Control.Monad.Reader (MonadReader)
import Data.Time.LocalTime  (TimeZone)
import Data.Time.Clock      (UTCTime)

import Common.Optics

data Env 
    = Env 
    { now :: !UTCTime
    , tz  :: !TimeZone
    }
  deriving (Eq, Ord, Show, Generic)

type MonadEnv m = MonadReader Env m
