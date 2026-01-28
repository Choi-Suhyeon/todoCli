module Common.Serialization (Serializable (..), UsingCereal (..), SerializationError (..)) where

import Data.ByteString (ByteString)
import Data.Serialize (Serialize (..), decode, encode)

import External.Prelude

class Serializable a where
    serialize :: a -> ByteString
    deserialize :: ByteString -> Either SerializationError a

newtype UsingCereal a = UsingCereal {unUsingCereal :: a}

instance (Serialize a) => Serializable (UsingCereal a) where
    serialize (UsingCereal x) = encode x
    deserialize = bimap DeserializationFailed UsingCereal . decode

data SerializationError = DeserializationFailed String

instance Show SerializationError where
    show (DeserializationFailed s) = "Deserialization failed: " <> s
