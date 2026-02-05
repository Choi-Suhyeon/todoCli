module Domain.Log (renderTaskDetail, renderTaskSummary, renderTaskConcise) where

import Data.HashSet (HashSet)
import Data.Text (Text)
import Data.Time.LocalTime (TimeZone, utcToLocalTime)
import Formatting (sformat, stext, (%), (%+))

import Data.HashSet qualified as HS
import Data.List qualified as L
import Data.Text qualified as T

import Domain.TaskDetail
import External.ISO8601 (iso8601ShowNoFrac)
import External.Prelude

renderTaskDetail :: TimeZone -> TaskDetail -> Text
renderTaskDetail tz TaskDetail{name, status, deadline, tags, memo, importance} =
    T.unlines
        [ "  name:       " <> name
        , "  status:     " <> into (show status)
        , "  deadline:   " <> renderDeadline tz deadline
        , "  importance: " <> into (show importance)
        , "  tags:       " <> renderTags False tags
        , "  memo:       " <> renderMemo memo
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

renderTaskConcise :: TimeZone -> TaskDetail -> Text
renderTaskConcise tz TaskDetail{name, deadline, tags, memo} =
    sformat
        ("[" % stext % "][" % stext % "]" %+ stext %+ "(memo:" %+ stext % ")")
        (renderDeadline tz deadline)
        (renderTags False tags)
        name
        (renderMemo memo)

renderDeadline :: TimeZone -> TaskDetailDeadline -> Text
renderDeadline _ DBoundless = "N/A"
renderDeadline tz (DBound d) = utcToLocalTime tz d & iso8601ShowNoFrac & T.pack

renderTags :: Bool -> HashSet Text -> Text
renderTags delimRequired tags
    | HS.null tags = "N/A"
    | otherwise =
        HS.toList tags
            & L.sort
            & T.intercalate " "
            & bool id (("(tags: " <>) . (<> ")")) delimRequired

renderMemo :: Text -> Text
renderMemo = liftA2 (bool "N/A") id (not . T.null)
