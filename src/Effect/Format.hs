module Effect.Format
    ( ColNameTaskSnapshot (..)
    , RenderConfig (..)
    , Column
    , initTaskSnapshotRenderConfig
    , sortTaskDetails
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
import Domain (TaskStatusDetail(..), TaskDetail(..))

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
    :: TimeZone -> Int -> Int -> RenderConfig ColNameTaskSnapshot TaskDetail
initTaskSnapshotRenderConfig tz vs hs =
    RenderConfig
        { vSpace = vs
        , hSpace = hs
        , cols =
            [ Column CNStatus $ getStatusSymbol @String . (\t -> t.status)
            , Column CNName $ T.unpack . (\t -> t.name)
            , Column CNDeadline $ iso8601Show . utcToLocalTime tz . (\t -> t.deadline)
            , Column CNTags $ T.unpack . T.intercalate ", " . sort . S.toList . (\t -> t.tags)
            , Column CNDesc $ T.unpack . (\t -> t.desc)
            ]
        }

sortTaskDetails :: [TaskDetail] -> [TaskDetail]
sortTaskDetails =
    sortBy
        $ comparing (\t -> t.status)
            <> comparing (Down . (\t -> t.deadline))
            <> comparing (length . (\t -> t.tags))
            <> comparing (T.length . (\t -> t.name))
            <> comparing (T.length . (\t -> t.desc))

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

getStatusSymbol :: (IsString a) => TaskStatusDetail -> a
getStatusSymbol DOverdue = "[X]"
getStatusSymbol DDue = "[!]"
getStatusSymbol DUndone = "[U]"
getStatusSymbol DDone = "[O]"
