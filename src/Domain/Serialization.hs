{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Serialization (Serializable(..), UsingCereal(..)) where

import Data.HashSet qualified as S
import Data.Text    qualified as T

import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Generics.Labels  ()
import Data.ByteString       (ByteString)
import Data.Serialize        (Serialize(..), encode, decode)
import Data.Bifunctor        (bimap)
import Data.Hashable         (Hashable)
import Data.HashSet          (HashSet)
import GHC.Generics          (Generic)
import Lens.Micro            ((^.))
import Data.Time             (UTCTime)
import Data.Text             (Text)

import Domain.Type.Internal
import Domain.Error

class Serializable a where
    serialize   :: a -> ByteString
    deserialize :: ByteString -> Either DomainError a

newtype UsingCereal a = UsingCereal { unUsingCereal :: a }  
  deriving (Show, Generic)

instance Serialize a => Serializable (UsingCereal a) where
    serialize   = encode . (^. #unUsingCereal)
    deserialize = bimap DeserializationFailed UsingCereal . decode 

deriving newtype instance Serialize TaskId
deriving anyclass instance Serialize TaskStatus
deriving anyclass instance Serialize Ids
deriving anyclass instance Serialize Task
deriving anyclass instance Serialize TodoRegistry

instance Serialize UTCTime where
    put = put . round @POSIXTime @Int . utcTimeToPOSIXSeconds  
    get = posixSecondsToUTCTime . fromIntegral @Int <$> get

instance Serialize Text where
    put = put . T.unpack
    get = T.pack <$> get
    
instance (Serialize a, Hashable a, Eq a) => Serialize (HashSet a) where
    put = put . S.toList
    get = S.fromList <$> get
