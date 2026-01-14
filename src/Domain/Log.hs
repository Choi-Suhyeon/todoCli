module Domain.Log (renderTaskDetail, renderTaskSummary) where

import Data.HashSet (HashSet)
import Data.Text (Text)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (TimeZone, utcToLocalTime)

import Data.HashSet qualified as HS
import Data.List qualified as L
import Data.Text qualified as T

import Common.Prelude
import Domain.TaskDetail

renderTaskDetail :: TimeZone -> TaskDetail -> Text
renderTaskDetail tz TaskDetail{name, status, deadline, tags, memo, importance} =
    T.unlines
        [ "  name:       " <> name
        , "  status:     " <> into (show status)
        , "  deadline:   " <> renderDeadline tz deadline
        , "  importance: " <> into (show importance)
        , "  tags:       " <> renderTags False tags
        , "  memo:       " <> if T.null memo then "N/A" else memo
        ]

renderTaskSummary :: TaskDetail -> Text
renderTaskSummary TaskDetail{name, status, tags} =
    mconcat
        [ into $ show status
        , " task '"
        , name
        , "' "
        , if HS.null tags then "" else renderTags True tags
        ]

renderDeadline :: TimeZone -> TaskDetailDeadline -> Text
renderDeadline _ DBoundless = "N/A"
renderDeadline tz (DBound d) = utcToLocalTime tz d & iso8601Show & T.pack

renderTags :: Bool -> HashSet Text -> Text
renderTags delimRequired tags
    | HS.null tags = "N/A"
    | otherwise =
        HS.toList tags
            & L.sort
            & T.intercalate " "
            & bool id (("(tags: " <>) . (<> ")")) delimRequired
