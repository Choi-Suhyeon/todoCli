module External.ISO8601 (module Data.Time.Format.ISO8601, iso8601ShowNoFrac) where

import Data.Time.Format.ISO8601

import External.Prelude
import External.Regex ((=~))

iso8601ShowNoFrac :: (ISO8601 t) => t -> String
iso8601ShowNoFrac = (\(x, _y, z) -> x <> z) . splitOnFracPart . iso8601Show
  where
    splitOnFracPart :: String -> (String, String, String)
    splitOnFracPart = (=~ ("\\.[0-9]+" :: String))
