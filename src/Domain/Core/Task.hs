module Domain.Core.Task
    ( -- * Public InBound Types
      EntryCreation (..)
    , EntryPatch (..)
    , EntryDeadline (..)
    , EntryStatus (..)

      -- * Public Outbound Types
    , TaskBasic (..)
    , TaskBasicStatus (..)
    , TaskBasicDeadline (..)
    , TaskDetail (..)
    , TaskDetailStatus (..)
    , TaskDetailDeadline (..)

      -- * Public(Domain) API
    , mkTask
    , modifyTask
    , toTaskDetail
    , toTaskBasic
    ) where

import Data.HashSet (HashSet)
import Data.Text (Text)
import Data.Time.Clock (UTCTime (..))
import Data.Time.LocalTime (TimeZone)

import Common.Prelude hiding (get, put)
import Domain.Core.Internal
import Domain.Core.Task.Internal
import Domain.Error

data EntryCreation = EntryCreation
    { name :: !Text
    , memo :: !Text
    , tags :: !(HashSet Text)
    , deadline :: !EntryDeadline
    }
    deriving (Show)

data EntryPatch = EntryPatch
    { name :: !(Maybe Text)
    , memo :: !(Maybe Text)
    , tags :: !(Maybe (HashSet Text))
    , deadline :: !(Maybe EntryDeadline)
    , status :: !(Maybe EntryStatus)
    }
    deriving (Show)

data EntryDeadline
    = EBoundless
    | EBound UTCTime
    deriving (Show)

instance From EntryDeadline TaskDeadline where
    from EBoundless = Boundless
    from (EBound t) = Bound t

data EntryStatus
    = EDone
    | EUndone

instance Show EntryStatus where
    show EDone = "done"
    show EUndone = "undone"

instance From EntryStatus TaskStatus where
    from EDone = Done
    from EUndone = Undone

instance From TaskStatus EntryStatus where
    from Done = EDone
    from Undone = EUndone

data TaskBasic = TaskBasic
    { name :: !Text
    , memo :: !Text
    , tags :: !(HashSet Text)
    , status :: !TaskBasicStatus
    , deadline :: TaskBasicDeadline
    }
    deriving (Show)

data TaskBasicStatus
    = BDone
    | BUndone
    deriving (Eq, Ord)

instance Show TaskBasicStatus where
    show BDone = "done"
    show BUndone = "undone"

instance From TaskStatus TaskBasicStatus where
    from Done = BDone
    from Undone = BUndone

instance From TaskBasicStatus TaskStatus where
    from BDone = Done
    from BUndone = Undone

data TaskBasicDeadline
    = BBoundless
    | BBound UTCTime
    deriving (Eq, Show)

instance From TaskDeadline TaskBasicDeadline where
    from Boundless = BBoundless
    from (Bound d) = BBound d

instance Ord TaskBasicDeadline where
    compare BBoundless BBoundless = EQ
    compare BBoundless (BBound _) = GT
    compare (BBound _) BBoundless = LT
    compare (BBound d1) (BBound d2) = d1 `compare` d2

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

instance From TaskDeadline TaskDetailDeadline where
    from Boundless = DBoundless
    from (Bound d) = DBound d

instance Ord TaskDetailDeadline where
    compare DBoundless DBoundless = EQ
    compare DBoundless (DBound _) = GT
    compare (DBound _) DBoundless = LT
    compare (DBound d1) (DBound d2) = d1 `compare` d2

mkTask :: TimeZone -> UTCTime -> EntryCreation -> Either DomainError Task
mkTask tz now EntryCreation{..}
    | EBound d <- deadline, Left e <- validateDeadline tz now d = Left e
    | otherwise =
        Right
            Task
                { name = into name
                , memo = into memo
                , tags = into tags
                , deadline = into deadline
                , status = Undone
                }

modifyTask
    :: TimeZone -> UTCTime -> EntryPatch -> Task -> Either DomainError Task
modifyTask tz now entry task
    | Just (EBound d) <- entry.deadline
    , Left e <- validateDeadline tz now d =
        Left e
    | otherwise =
        Right
            Task
                { name = fromMaybe task.name $ into <$> entry.name
                , memo = fromMaybe task.memo $ into <$> entry.memo
                , tags = fromMaybe task.tags $ into <$> entry.tags
                , deadline = fromMaybe task.deadline (into <$> entry.deadline)
                , status = maybe task.status into entry.status
                }

toTaskBasic :: Task -> TaskBasic
toTaskBasic Task{..} =
    TaskBasic
        { name = into name
        , memo = into memo
        , tags = into tags
        , status = into status
        , deadline = into deadline
        }

toTaskDetail :: UTCTime -> Task -> TaskDetail
toTaskDetail now Task{..} =
    TaskDetail
        { name = into name
        , memo = into memo
        , tags = into tags
        , status = enrichStatus status
        , deadline = into deadline
        }
  where
    enrichStatus :: TaskStatus -> TaskDetailStatus
    enrichStatus Done = DDone
    enrichStatus Undone = case deadline of
        Boundless -> DUndone
        Bound d
            | isOverdue now d -> DOverdue
            | isDue now d -> DDue
            | otherwise -> DUndone
