module Common.Env.Runtime (Runtime (..)) where

import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeZone)

import Common.Prelude

data Runtime = Runtime
    { now :: !UTCTime
    , tz :: !TimeZone
    }
    deriving (Eq, Ord, Show)
