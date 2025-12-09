{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Serialization (Serializable (..), UsingCereal (..)) where

import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.Generics.Labels ()
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize (..), decode, encode)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX
    ( POSIXTime
    , posixSecondsToUTCTime
    , utcTimeToPOSIXSeconds
    )

import Data.HashSet qualified as S
import Data.Text qualified as T

import Common.Optics
import Domain.Error
import Domain.TaskId
import Domain.TodoRegistry

class Serializable a where
    serialize :: a -> ByteString
    deserialize :: ByteString -> Either DomainError a

newtype UsingCereal a = UsingCereal {unUsingCereal :: a}
    deriving (Generic, Show)

instance (Serialize a) => Serializable (UsingCereal a) where
    serialize = encode . (^. #unUsingCereal)
    deserialize = bimap DeserializationFailed UsingCereal . decode

deriving newtype instance Serialize TaskId
deriving anyclass instance Serialize TaskStatus
deriving anyclass instance Serialize Deadline
deriving anyclass instance Serialize Ids
deriving anyclass instance Serialize Task
deriving anyclass instance Serialize TodoRegistry

instance Serialize UTCTime where
    put = put . round @POSIXTime @Int . utcTimeToPOSIXSeconds
    get = posixSecondsToUTCTime . fromIntegral @Int <$> get

instance Serialize Text where
    put = put . T.unpack
    get = T.pack <$> get

instance (Eq a, Hashable a, Serialize a) => Serialize (HashSet a) where
    put = put . S.toList
    get = S.fromList <$> get
