module Domain.Core.Internal
    ( isDue
    , isOverdue
    , validateDeadline
    ) where

import Data.Fixed (Pico)
import Data.Time.Clock (NominalDiffTime, UTCTime (..), addUTCTime)
import Data.Time.LocalTime (TimeZone, utcToLocalTime)

import Common.Prelude
import Domain.Error

isDue :: UTCTime -> UTCTime -> Bool
isDue now = liftA2 (&&) (> now) (<= addUTCTime statusDueThreshold now)

isOverdue :: UTCTime -> UTCTime -> Bool
isOverdue now = (<= now)

validateDeadline :: TimeZone -> UTCTime -> UTCTime -> Either DomainError ()
validateDeadline tz now dd
    | dd > now = Right ()
    | otherwise =
        let
            nowL = utcToLocalTime tz now
            ddL = utcToLocalTime tz dd
         in
            Left $ InvalidDeadline nowL ddL

statusDueThreshold :: NominalDiffTime
statusDueThreshold = from @Pico $ 48 * 3600
