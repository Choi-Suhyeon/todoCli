module View (TaskDetailRenderConfig, initTaskDetailRenderConfig, sortTaskDetails) where

import Data.List (sort, sortBy)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (TimeZone, utcToLocalTime)
import Text.Layout.Table
    ( column
    , expandBetween
    , fixed
    , fixedUntil
    , left
    , noAlign
    , noCutMark
    )
import Witch

import Data.Text qualified as T

import Common.Prelude
import Domain
    ( TaskDetail (..)
    , TaskDetailDeadline (..)
    , TaskDetailStatus (..)
    )
import Effect.Format

data ColNameTaskDetail
    = CNStatus
    | CNDeadline
    | CNTags
    | CNName
    | CNMemo
    | CNImportance
    deriving (Eq)

instance Show ColNameTaskDetail where
    show CNStatus = "ST."
    show CNDeadline = "Deadline"
    show CNTags = "Tags"
    show CNName = "Name"
    show CNMemo = "Memo"
    show CNImportance = "IMP."

type TaskDetailRenderConfig = RenderConfig TaskDetail

initTaskDetailRenderConfig :: TimeZone -> Text -> TaskDetailRenderConfig
initTaskDetailRenderConfig tz moreInfo =
    RenderConfig
        { moreInfo
        , cols =
            [ Column "ST." renderStatus $ column (fixed 3) left noAlign noCutMark
            , Column "IMP." renderImportance $ column (fixed 4) left noAlign noCutMark
            , Column "Name" renderName $ column (expandBetween 4 30) left noAlign noCutMark
            , Column "Deadline" renderDeadline $ column (fixedUntil 8) left noAlign noCutMark
            , Column "Tags" renderTags $ column (fixedUntil 4) left noAlign noCutMark
            , Column "Memo" renderMemo $ column (expandBetween 4 60) left noAlign noCutMark
            ]
        }
  where
    renderStatus :: TaskDetail -> Text
    renderStatus TaskDetail{status = DOverdue} = "[X]"
    renderStatus TaskDetail{status = DDue} = "[!]"
    renderStatus TaskDetail{status = DUndone} = "[U]"
    renderStatus TaskDetail{status = DDone} = "[O]"

    renderName :: TaskDetail -> Text
    renderName TaskDetail{name = n} = n

    renderDeadline :: TaskDetail -> Text
    renderDeadline TaskDetail{deadline = DBoundless} = "N/A"
    renderDeadline TaskDetail{deadline = DBound d} = utcToLocalTime tz d & iso8601Show & into

    renderTags :: TaskDetail -> Text
    renderTags TaskDetail{tags = ts}
        | null ts = "N/A"
        | otherwise = toList ts & sort & T.intercalate ", "

    renderMemo :: TaskDetail -> Text
    renderMemo TaskDetail{memo = m}
        | T.null m = "N/A"
        | otherwise = into m

    renderImportance :: TaskDetail -> Text
    renderImportance TaskDetail{importance = i} = into $ show i

sortTaskDetails :: [TaskDetail] -> [TaskDetail]
sortTaskDetails =
    sortBy
        $ comparing (.status)
        <> comparing (.importance)
        <> comparing (Down . (.deadline))
        <> comparing (length . (.tags))
        <> comparing (T.length . (.name))
        <> comparing (T.length . (.memo))
