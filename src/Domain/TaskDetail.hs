module Domain.TaskDetail
    ( TaskDetail (..)
    , TaskDetailStatus (..)
    , TaskDetailDeadline (..)
    , toTaskDetail
    , toTaskDetailFromBasic
    , isDue
    , isOverdue
    ) where

import Data.Fixed (Pico)
import Data.HashSet (HashSet)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime)

import Common.Prelude
import Domain.Core

data TaskDetail = TaskDetail
    { name :: !Text
    , memo :: !Text
    , tags :: !(HashSet Text)
    , status :: !TaskDetailStatus
    , deadline :: !TaskDetailDeadline
    }
    deriving (Show)

data TaskDetailStatus
    = DDone
    | DUndone
    | DDue
    | DOverdue
    deriving (Eq, Ord)

instance Show TaskDetailStatus where
    show DDone = "done"
    show DUndone = "undone"
    show DDue = "due"
    show DOverdue = "overdue"

data TaskDetailDeadline
    = DBoundless
    | DBound UTCTime
    deriving (Eq, Show)

instance From TaskBasicDeadline TaskDetailDeadline where
    from BBoundless = DBoundless
    from (BBound d) = DBound d

instance Ord TaskDetailDeadline where
    compare DBoundless DBoundless = EQ
    compare DBoundless (DBound _) = GT
    compare (DBound _) DBoundless = LT
    compare (DBound d1) (DBound d2) = d1 `compare` d2

toTaskDetail :: UTCTime -> Task -> TaskDetail
toTaskDetail now = toTaskDetailFromBasic now . toTaskBasic

toTaskDetailFromBasic :: UTCTime -> TaskBasic -> TaskDetail
toTaskDetailFromBasic now TaskBasic{..} =
    TaskDetail
        { name = name
        , memo = memo
        , tags = tags
        , deadline = into deadline
        , status = enrichStatus status
        }
  where
    enrichStatus :: TaskBasicStatus -> TaskDetailStatus
    enrichStatus BDone = DDone
    enrichStatus BUndone = case deadline of
        BBoundless -> DUndone
        BBound d
            | isOverdue now d -> DOverdue
            | isDue now d -> DDue
            | otherwise -> DUndone

isDue :: UTCTime -> UTCTime -> Bool
isDue now = liftA2 (&&) (> now) (<= addUTCTime statusDueThreshold now)

isOverdue :: UTCTime -> UTCTime -> Bool
isOverdue now = (<= now)

statusDueThreshold :: NominalDiffTime
statusDueThreshold = from @Pico $ 48 * 3600
