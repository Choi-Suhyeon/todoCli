module Common.Env.Config.Error (ConfigError (..)) where

import Toml (TomlDecodeError)

import External.Prelude

data ConfigError
    = TomlDecodingFailed [TomlDecodeError]
    | Utf8DecodingFailed

instance Show ConfigError where
    show (TomlDecodingFailed es) = "failed to decode TOML config:\n" <> unlines (show <$> es)
    show Utf8DecodingFailed = "failed to decode config as UTF-8."
