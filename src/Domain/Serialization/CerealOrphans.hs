{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Serialization.CerealOrphans () where

import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize (..))
import Data.Text (Text)
import Data.Time.Clock (UTCTime (..))
import Data.Time.Clock.POSIX
    ( POSIXTime
    , posixSecondsToUTCTime
    , utcTimeToPOSIXSeconds
    )

import Data.HashSet qualified as HS
import Data.Text qualified as T

instance Serialize UTCTime where
    put = put . round @POSIXTime @Int . utcTimeToPOSIXSeconds
    get = posixSecondsToUTCTime . fromIntegral @Int <$> get

instance Serialize Text where
    put = put . T.unpack
    get = T.pack <$> get

instance (Eq a, Hashable a, Serialize a) => Serialize (HashSet a) where
    put = put . HS.toList
    get = HS.fromList <$> get
