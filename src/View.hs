module View
    ( TaskDetailRenderConfig
    , ViewColName (..)
    , initTaskDetailRenderConfigWith
    , initTaskDetailRenderConfig
    , sortTaskDetails
    , renderTable
    ) where

import Data.List (sort, sortBy)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
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

import Data.Text qualified as T

import Domain
    ( TaskDetail (..)
    , TaskDetailDeadline (..)
    , TaskDetailStatus (..)
    , memoLenBound
    , nameLenBound
    )
import External.ISO8601 (iso8601ShowNoFrac)
import External.Interval (Interval, fromFinite, upperIntegralBound)
import External.Prelude
import View.Format

data ViewColName
    = CNStatus
    | CNDeadline
    | CNTags
    | CNName
    | CNMemo
    | CNImportance
    deriving (Eq)

instance Show ViewColName where
    show CNStatus = "ST."
    show CNDeadline = "Deadline"
    show CNTags = "Tags"
    show CNName = "Name"
    show CNMemo = "Memo"
    show CNImportance = "IMP."

type TaskDetailRenderConfig = RenderConfig ViewColName TaskDetail

initTaskDetailRenderConfigWith
    :: (ViewColName -> Bool) -> TimeZone -> Text -> TaskDetailRenderConfig
initTaskDetailRenderConfigWith p tz moreInfo = initConfig{cols = filter (p . (.name)) initConfig.cols}
  where
    initConfig = initTaskDetailRenderConfig tz moreInfo

initTaskDetailRenderConfig :: TimeZone -> Text -> TaskDetailRenderConfig
initTaskDetailRenderConfig tz moreInfo =
    RenderConfig
        { moreInfo
        , cols =
            [ Column CNStatus renderStatus
                $ column (fixed 3) left noAlign noCutMark
            , Column CNImportance renderImportance
                $ column (fixed 4) left noAlign noCutMark
            , Column CNName renderName
                $ column (expandBetween 4 $ getMaxLen nameLenBound) left noAlign noCutMark
            , Column CNDeadline renderDeadline
                $ column (fixedUntil 8) left noAlign noCutMark
            , Column CNTags renderTags
                $ column (fixedUntil 4) left noAlign noCutMark
            , Column CNMemo renderMemo
                $ column (expandBetween 4 $ getMaxLen memoLenBound) left noAlign noCutMark
            ]
        }
  where
    getMaxLen :: Interval Int -> Int
    getMaxLen = fromJust . fromFinite . upperIntegralBound

    renderStatus :: TaskDetail -> Text
    renderStatus TaskDetail{status = DOverdue} = "[X]"
    renderStatus TaskDetail{status = DDue} = "[!]"
    renderStatus TaskDetail{status = DUndone} = "[U]"
    renderStatus TaskDetail{status = DDone} = "[O]"

    renderName :: TaskDetail -> Text
    renderName TaskDetail{name = n} = n

    renderDeadline :: TaskDetail -> Text
    renderDeadline TaskDetail{deadline = DBoundless} = "N/A"
    renderDeadline TaskDetail{deadline = DBound d} = utcToLocalTime tz d & iso8601ShowNoFrac & into

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
