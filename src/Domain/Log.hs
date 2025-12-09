module Domain.Log (MonadLog, Log, logMsg, renderTaskDetail, renderTaskSummary) where

import Control.Monad.Writer.Strict (MonadWriter, tell)
import Data.Bool (bool)
import Data.HashSet (HashSet)
import Data.List (sort)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (TimeZone, utcToLocalTime)
import Witch

import Data.HashSet qualified as S
import Data.List qualified as L
import Data.Text qualified as T

import Common
import Domain.TodoRegistry

type MonadLog m = MonadWriter Log m

type Log = Seq Text

logMsg :: (MonadLog m) => Text -> m ()
logMsg = tell . into . L.singleton . ("[I] " <>) . (<> "\n")

renderTaskDetail :: TimeZone -> TaskDetail -> Text
renderTaskDetail tz TaskDetail{name, status, deadline, tags, memo} =
    T.unlines
        [ "  name:     " <> name
        , "  status:   " <> into (show status)
        , "  deadline: " <> renderDeadline tz deadline
        , "  tags:     " <> renderTags False tags
        , "  memo:     " <> if T.null memo then "N/A" else memo
        ]

renderTaskSummary :: TaskDetail -> Text
renderTaskSummary TaskDetail{name, status, tags} =
    T.concat [into $ show status, " task '", name, "' ", renderTags True tags]

renderDeadline :: TimeZone -> TaskDetailDeadline -> Text
renderDeadline _ DBoundless = "N/A"
renderDeadline tz (DBound d) = utcToLocalTime tz d & iso8601Show & T.pack

renderTags :: Bool -> HashSet Text -> Text
renderTags delimRequired tags
    | S.null tags = "N/A"
    | otherwise =
        S.toList tags
            & sort
            & T.intercalate " "
            & bool id (("(tags: " <>) . (<> ")")) delimRequired
