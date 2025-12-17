module Domain.Serialization (Serializable (..), UsingCereal (..)) where

import Data.ByteString (ByteString)
import Data.Serialize (Serialize (..), decode, encode)

import Common.Prelude
import Domain.Error

class Serializable a where
    serialize :: a -> ByteString
    deserialize :: ByteString -> Either DomainError a

newtype UsingCereal a = UsingCereal {unUsingCereal :: a}

instance (Serialize a) => Serializable (UsingCereal a) where
    serialize (UsingCereal x) = encode x
    deserialize = bimap DeserializationFailed UsingCereal . decode
