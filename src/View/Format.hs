module View.Format (Column (..), RenderConfig (..), renderTable) where

import Data.Text (Text)
import Text.Layout.Table (ColSpec, Row, concatGrid, gridB)
import Text.Layout.Table.Cell.WideString (WideText (..))

import Data.Text qualified as T

import External.Prelude

data Column a b = Column
    { name :: a
    , render :: b -> Text
    , spec :: ColSpec
    }

data RenderConfig a b = RenderConfig
    { cols :: [Column a b]
    , moreInfo :: Text
    }

renderTable :: forall a b. (Show a) => RenderConfig a b -> [b] -> Text
renderTable RenderConfig{..} = renderWithInfo . (:) wideTextHeaders . fmap toBody
  where
    renderWithInfo :: [Row WideText] -> Text
    renderWithInfo = (<> info) . concatGrid 2 . gridB specs
      where
        info = liftA3 bool ("\n" <>) id T.null moreInfo

    toBody :: b -> [WideText]
    toBody = (<$> rendersToWideText) . (&)

    rendersToWideText :: [b -> WideText]
    rendersToWideText = map (WideText .) renders

    wideTextHeaders :: [WideText]
    wideTextHeaders = map WideText headers

    renders :: [b -> Text]
    headers :: [Text]
    specs :: [ColSpec]

    (renders, headers, specs) =
        unzip3 $ liftA3 (,,) (.render) (into . show . (.name)) (.spec) <$> cols
