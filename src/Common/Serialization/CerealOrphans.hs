{-# OPTIONS_GHC -Wno-orphans #-}

module Common.Serialization.CerealOrphans () where

import Data.Fixed (Pico)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.Serialize (Serialize (..))
import Data.Text (Text)
import Data.Time.Calendar (Day (..))
import Data.Time.Clock (DiffTime, UTCTime (..))

import Data.HashSet qualified as HS
import Data.Text qualified as T

import Common.Prelude hiding (get, put)

instance Serialize Text where
    put = put . T.unpack
    get = T.pack <$> get

instance (Eq a, Hashable a, Serialize a) => Serialize (HashSet a) where
    put = put . HS.toList
    get = HS.fromList <$> get

instance Serialize UTCTime where
    put (UTCTime day dt) = do
        put $ fromIntegral @_ @Int64 $ toModifiedJulianDay day
        put $ diffTimeToNanos dt

    get = do
        mjd <- get @Int64
        ns <- get @Int64

        if
            | 0 <= ns && ns < maxNs
            , let
                day = ModifiedJulianDay (fromIntegral mjd)
            , let
                diffTime = nanosToDiffTime ns ->
                pure $ UTCTime day diffTime
            | otherwise ->
                fail $ "UTCTime: ns out of range " <> show ns

maxNs :: Int64
maxNs = 86_400 * 1_000_000_000

diffTimeToNanos :: DiffTime -> Int64
diffTimeToNanos dt = truncate $ realToFrac dt * (1_000_000_000 :: Pico)

nanosToDiffTime :: Int64 -> DiffTime
nanosToDiffTime ns = realToFrac $ fromIntegral ns / (1_000_000_000 :: Pico)
