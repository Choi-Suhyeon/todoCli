module Effect.Format
    ( ColNameTaskSnapshot (..)
    , RenderConfig (..)
    , Column
    , initTaskSnapshotRenderConfig
    , sortTaskSnapshots
    , renderTable
    , renderTableWithout
    ) where

import Data.List (sort, sortBy)
import Data.Ord (Down (..), comparing)
import Data.String (IsString)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (TimeZone, utcToLocalTime)
import Text.PrettyPrint.Boxes (Box, hsep, left, render, text, top, vsep)

import Data.HashSet qualified as S
import Data.Text qualified as T

import Common
import Domain (SnapshotStatus (..), TaskSnapshot (..))

data Column a b = Column
    { colName :: a
    , extractColStr :: b -> String
    }
    deriving (Generic)

data RenderConfig a b = RenderConfig
    { cols :: [Column a b]
    , vSpace :: Int
    , hSpace :: Int
    }
    deriving (Generic)

data ColNameTaskSnapshot
    = CNStatus
    | CNDeadline
    | CNTags
    | CNName
    | CNDesc
    deriving (Eq, Show)

initTaskSnapshotRenderConfig
    :: TimeZone -> Int -> Int -> RenderConfig ColNameTaskSnapshot TaskSnapshot
initTaskSnapshotRenderConfig tz vs hs =
    RenderConfig
        { vSpace = vs
        , hSpace = hs
        , cols =
            [ Column CNStatus $ getStatusSymbol @String . (^. #statusS)
            , Column CNName $ T.unpack . (^. #nameS)
            , Column CNDeadline $ iso8601Show . utcToLocalTime tz . (^. #deadlineS)
            , Column CNTags $ T.unpack . T.intercalate " " . sort . S.toList . (^. #tagsS)
            , Column CNDesc $ T.unpack . (^. #descS)
            ]
        }

sortTaskSnapshots :: [TaskSnapshot] -> [TaskSnapshot]
sortTaskSnapshots =
    sortBy
        $ comparing (^. #statusS)
            <> comparing (Down . (^. #deadlineS))
            <> comparing (length . (^. #tagsS))
            <> comparing (T.length . (^. #nameS))
            <> comparing (T.length . (^. #descS))

renderTable :: (Eq a) => RenderConfig a b -> [b] -> String
renderTable = renderTableWithout Nothing

renderTableWithout
    :: forall a b f. (Eq a, Foldable f) => f a -> RenderConfig a b -> [b] -> String
renderTableWithout xs RenderConfig{..} ts =
    cols
        & filter ((`notElem` xs) . (^. #colName))
        & map (text .: (^. #extractColStr))
        & flip (renderTable' vSpace hSpace) ts
  where
    renderTable' :: Int -> Int -> [b -> Box] -> [b] -> String
    renderTable' vs hs = (render . hsep hs top . (vsep vs left <$>)) .: weave

getStatusSymbol :: (IsString a) => SnapshotStatus -> a
getStatusSymbol SOverdue = "[X]"
getStatusSymbol SDue = "[!]"
getStatusSymbol SUndone = "[U]"
getStatusSymbol SDone = "[O]"
