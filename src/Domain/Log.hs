module Domain.Log (MonadLog, Log, logMsg, renderTaskDetail, renderTaskSummary) where

import Control.Monad.Reader (ask)
import Control.Monad.Writer.Strict (MonadWriter, tell)
import Data.Bool (bool)
import Data.HashSet (HashSet)
import Data.List (sort)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (utcToLocalTime)
import Witch

import Data.HashSet qualified as S
import Data.List qualified as L
import Data.Text qualified as T

import Common
import Domain.Internal
import Domain.Type.Internal

type MonadLog m = MonadWriter Log m

type Log = Seq Text

logMsg :: (MonadLog m) => Text -> m ()
logMsg = tell . into . L.singleton . ("[I] " <>) . (<> "\n")

renderTaskDetail :: (MonadEnv m) => Task -> m Text
renderTaskDetail Task{name, status, deadline, tags, desc} = do
    Env{tz, now} <- ask

    pure . T.unlines
        $ [ "  name:     " <> name
          , "  status:   " <> renderStatus now deadline status
          , "  deadline: " <> (deadline & utcToLocalTime tz & iso8601Show & T.pack)
          , "  tags:     " <> renderTags False tags
          , "  note:     " <> desc
          ]

renderTaskSummary :: (MonadEnv m) => Task -> m Text
renderTaskSummary Task{name, deadline, status, tags} =
    ask >>= \Env{now} -> pure $ T.concat [renderStatus now deadline status, " task '", name, "' ", renderTags True tags]

renderTags :: Bool -> HashSet Text -> Text
renderTags delimRequired tags
    | S.null tags = "(no tags)"
    | otherwise =
        S.toList tags
            & sort
            & T.intercalate " "
            & bool id (("(tags: " <>) . (<> ")")) delimRequired

renderStatus :: UTCTime -> UTCTime -> TaskStatus -> Text
renderStatus _ _ Done = "done"
renderStatus now deadline Undone =
    if
        | isOverdue now deadline -> "overdue"
        | isDue now deadline -> "due"
        | otherwise -> "undone"
