{-# LANGUAGE ImplicitParams #-}

module Common.Env.Config (HasConfig, Config (..), ConfigError (..), initConfig, parseConfig) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Toml (Codec, TomlCodec, (.=))

import Data.Text.Encoding qualified as TE
import Toml qualified

import Common.Env.Config.Error
import Common.Prelude

type HasConfig = ?config :: Config

data Config = Config
    { importanceDefault :: !Word
    , dueWithinHours :: !Word
    }
    deriving (Eq, Ord, Show)

initConfig :: Config
initConfig = Config{importanceDefault = 4, dueWithinHours = 48}

parseConfig :: ByteString -> Either ConfigError Config
parseConfig = decodeUtf8 >=> decodeToml
  where
    decodeUtf8 :: ByteString -> Either ConfigError Text
    decodeUtf8 = first (const Utf8DecodingFailed) . TE.decodeUtf8'

    decodeToml :: Text -> Either ConfigError Config
    decodeToml = first TomlDecodingFailed . Toml.decode configCodec

configCodec :: TomlCodec Config
configCodec = Config <$> importanceDefault <*> dueWithinHours
  where
    importanceDefault, dueWithinHours :: Codec Config Word

    importanceDefault = Toml.word "importance_default" .= (.importanceDefault)
    dueWithinHours = Toml.word "due_within_hours" .= (.dueWithinHours)
