module View (initTaskDetailRenderConfig, sortTaskDetails) where

import Data.List (sort, sortBy)
import Data.Ord (Down (..), comparing)
import Data.String (IsString)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (TimeZone, utcToLocalTime)

import Data.HashSet qualified as S
import Data.Text qualified as T

import Domain (TaskDetail (..), TaskStatusDetail (..))
import Effect.Format

data ColNameTaskDetail
    = CNStatus
    | CNDeadline
    | CNTags
    | CNName
    | CNDesc
    deriving (Eq)

instance Show ColNameTaskDetail where
    show CNStatus = "ST."
    show CNDeadline = "Deadline"
    show CNTags = "Tags"
    show CNName = "Name"
    show CNDesc = "Note"

initTaskDetailRenderConfig
    :: TimeZone -> RenderConfig ColNameTaskDetail TaskDetail
initTaskDetailRenderConfig tz =
    RenderConfig
        { vSpace = 0
        , cellMinWidth = 6
        , cols =
            [ Column CNStatus 1 False $ getStatusSymbol @String . (\t -> t.status)
            , Column CNName 2 False $ T.unpack . (\t -> t.name)
            , Column CNDeadline 1 False $ iso8601Show . utcToLocalTime tz . (\t -> t.deadline)
            , Column CNTags 2 True
                $ T.unpack . T.intercalate ", " . sort . S.toList . (\t -> t.tags)
            , Column CNDesc 2 True $ T.unpack . (\t -> t.desc)
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

getStatusSymbol :: (IsString a) => TaskStatusDetail -> a
getStatusSymbol DOverdue = "[X]"
getStatusSymbol DDue = "[!]"
getStatusSymbol DUndone = "[U]"
getStatusSymbol DDone = "[O]"
