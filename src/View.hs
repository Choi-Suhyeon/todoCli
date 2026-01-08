module View (initTaskDetailRenderConfig, sortTaskDetails) where

import Data.List (sort, sortBy)
import Data.Ord (Down (..), comparing)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (TimeZone, utcToLocalTime)
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

initTaskDetailRenderConfig
    :: TimeZone -> RenderConfig ColNameTaskDetail TaskDetail
initTaskDetailRenderConfig tz =
    RenderConfig
        { vSpace = 0
        , cellMinWidth = 6
        , cols =
            [ Column CNStatus 1 False renderStatus
            , Column CNImportance 1 False renderImportance
            , Column CNName 2 False renderName
            , Column CNDeadline 2 True renderDeadline
            , Column CNTags 2 True renderTags
            , Column CNMemo 2 True renderMemo
            ]
        }
  where
    renderStatus :: TaskDetail -> String
    renderStatus TaskDetail{status = DOverdue} = "[X]"
    renderStatus TaskDetail{status = DDue} = "[!]"
    renderStatus TaskDetail{status = DUndone} = "[U]"
    renderStatus TaskDetail{status = DDone} = "[O]"

    renderName :: TaskDetail -> String
    renderName TaskDetail{name = n} = into n

    renderDeadline :: TaskDetail -> String
    renderDeadline TaskDetail{deadline = DBoundless} = "N/A"
    renderDeadline TaskDetail{deadline = DBound d} = d & utcToLocalTime tz & iso8601Show

    renderTags :: TaskDetail -> String
    renderTags TaskDetail{tags = ts}
        | null ts = "N/A"
        | otherwise = toList ts & sort & T.intercalate ", " & into

    renderMemo :: TaskDetail -> String
    renderMemo TaskDetail{memo = m}
        | T.null m = "N/A"
        | otherwise = into m

    renderImportance :: TaskDetail -> String
    renderImportance TaskDetail{importance = i} = show i

sortTaskDetails :: [TaskDetail] -> [TaskDetail]
sortTaskDetails =
    sortBy
        $ comparing (.status)
        <> comparing (.importance)
        <> comparing (Down . (.deadline))
        <> comparing (length . (.tags))
        <> comparing (T.length . (.name))
        <> comparing (T.length . (.memo))
