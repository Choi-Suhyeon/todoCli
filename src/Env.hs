module Env (Env(..), MonadEnv) where

import Control.Monad.Reader (MonadReader)
import Data.Time.Clock      (UTCTime)
import GHC.Generics         (Generic)

data Env = Env { now :: !UTCTime }
  deriving (Eq, Ord, Show, Generic)

type MonadEnv m = MonadReader Env m
