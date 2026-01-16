module Effect.Format
    ( Column (..)
    , RenderConfig (..)
    , renderTable
    ) where

import Data.Text (Text)
import Text.Layout.Table (ColSpec, Row, concatGrid, gridB)

import Data.Text qualified as T

import Common.Prelude

data Column a = Column
    { name :: Text
    , render :: a -> Text
    , spec :: ColSpec
    }

data RenderConfig a = RenderConfig
    { cols :: [Column a]
    , moreInfo :: Text
    }

renderTable :: forall a. RenderConfig a -> [a] -> Text
renderTable RenderConfig{..} = renderWithInfo . (:) headers . fmap toBody
  where
    renderWithInfo :: [Row Text] -> Text
    renderWithInfo = (<> info) . concatGrid 2 . gridB specs
      where
        info = liftA3 bool ("\n" <>) id T.null moreInfo

    toBody :: a -> [Text]
    toBody = (<$> renders) . (&)

    renders :: [a -> Text]
    headers :: [Text]
    specs :: [ColSpec]

    (renders, headers, specs) =
        unzip3 $ liftA3 (,,) (.render) (.name) (.spec) <$> cols
