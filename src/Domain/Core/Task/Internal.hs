module Domain.Core.Task.Internal
    ( -- * Internal
      TaskStatus
    , TaskDeadline

      -- * Public Types
    , Task

      -- * Public InBound Types
    , EntryCreation (..)
    , EntryPatch (..)
    , EntryDeadline (..)
    , EntryStatus (..)

      -- * Public Outbound Types
    , TaskBasic (..)
    , TaskBasicStatus (..)
    , TaskBasicDeadline (..)

      -- * Public(Domain) API
    , mkTask
    , modifyTask
    , toTaskBasic
    ) where

import Data.HashSet (HashSet)
import Data.Text (Text)
import Data.Time.Clock (UTCTime (..))
import Data.Time.LocalTime (TimeZone, utcToLocalTime)

import Common.Prelude hiding (get, put)
import Domain.Core.Task.Raw
import Domain.Error

data EntryCreation = EntryCreation
    { name :: !Text
    , memo :: !Text
    , tags :: !(HashSet Text)
    , deadline :: !EntryDeadline
    , importance :: !Word
    }
    deriving (Show)

data EntryPatch = EntryPatch
    { name :: !(Maybe Text)
    , memo :: !(Maybe Text)
    , tags :: !(Maybe (HashSet Text))
    , deadline :: !(Maybe EntryDeadline)
    , status :: !(Maybe EntryStatus)
    , importance :: !(Maybe Word)
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
    , importance :: Word
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

mkTask :: TimeZone -> UTCTime -> EntryCreation -> Either DomainError Task
mkTask tz now EntryCreation{..}
    | EBound d <- deadline
    , Left e <- validateDeadline tz now d =
        Left e
    | Left e <- validateImportance importance = Left e
    | otherwise =
        Right
            Task
                { name = into name
                , memo = into memo
                , tags = into tags
                , deadline = into deadline
                , importance = into importance
                , status = Undone
                }

modifyTask
    :: TimeZone -> UTCTime -> EntryPatch -> Task -> Either DomainError Task
modifyTask tz now entry task
    | Just (EBound d) <- entry.deadline
    , Left e <- validateDeadline tz now d =
        Left e
    | Just w <- entry.importance
    , Left e <- validateImportance w =
        Left e
    | otherwise =
        Right
            Task
                { name = maybe task.name into entry.name
                , memo = maybe task.memo into entry.memo
                , tags = maybe task.tags into entry.tags
                , status = maybe task.status into entry.status
                , deadline = maybe task.deadline into entry.deadline
                , importance = maybe task.importance into entry.importance
                }

toTaskBasic :: Task -> TaskBasic
toTaskBasic Task{..} =
    TaskBasic
        { name = into name
        , memo = into memo
        , tags = into tags
        , status = into status
        , deadline = into deadline
        , importance = into importance
        }

validateDeadline :: TimeZone -> UTCTime -> UTCTime -> Either DomainError ()
validateDeadline tz now dd
    | dd > now = Right ()
    | otherwise =
        let
            nowL = utcToLocalTime tz now
            ddL = utcToLocalTime tz dd
         in
            Left $ InvalidDeadline nowL ddL

validateImportance :: Word -> Either DomainError ()
validateImportance =
    bool (Left ImportanceOutOfRange) (Right ())
        . liftA2 (&&) (>= minBound) (<= maxBound)
        . into @TaskImportance
