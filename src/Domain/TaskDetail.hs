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
    , importance :: !Word
    }
    deriving (Show)

data TaskDetailStatus
    = DDone
    | DUndone
    | DOverdue
    | DDue
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

toTaskDetail :: Word -> UTCTime -> Task -> TaskDetail
toTaskDetail dueWithinHours now = toTaskDetailFromBasic dueWithinHours now . toTaskBasic

toTaskDetailFromBasic :: Word -> UTCTime -> TaskBasic -> TaskDetail
toTaskDetailFromBasic dueWithinHours now TaskBasic{..} =
    TaskDetail
        { name = name
        , memo = memo
        , tags = tags
        , importance = importance
        , deadline = into deadline
        , status = enrichStatus status
        }
  where
    threshold :: NominalDiffTime
    threshold = getStatusDueThreshold dueWithinHours

    enrichStatus :: TaskBasicStatus -> TaskDetailStatus
    enrichStatus BDone = DDone
    enrichStatus BUndone = case deadline of
        BBoundless -> DUndone
        BBound d
            | isOverdue now d -> DOverdue
            | isDue' threshold now d -> DDue
            | otherwise -> DUndone

isDue :: Word -> UTCTime -> UTCTime -> Bool
isDue = isDue' . getStatusDueThreshold

isOverdue :: UTCTime -> UTCTime -> Bool
isOverdue now = (<= now)

isDue' :: NominalDiffTime -> UTCTime -> UTCTime -> Bool
isDue' threshold now = liftA2 (&&) (> now) (<= addUTCTime threshold now)

getStatusDueThreshold :: Word -> NominalDiffTime
getStatusDueThreshold dueWithinHours = from @Pico $ 3600 * fromIntegral dueWithinHours
