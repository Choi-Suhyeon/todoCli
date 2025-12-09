module Domain.Serialization (Serializable (..), UsingCereal (..)) where

import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.Generics.Labels ()
import Data.Serialize (Serialize (..), decode, encode)

import Common.Optics
import Domain.Error

class Serializable a where
    serialize :: a -> ByteString
    deserialize :: ByteString -> Either DomainError a

newtype UsingCereal a = UsingCereal {unUsingCereal :: a}
    deriving (Generic, Show)

instance (Serialize a) => Serializable (UsingCereal a) where
    serialize (UsingCereal x) = encode x
    deserialize = bimap DeserializationFailed UsingCereal . decode
