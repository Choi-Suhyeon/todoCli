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

      -- * Public Value
    , nameLenBound
    , memoLenBound

      -- * Public(Domain) API
    , mkTask
    , modifyTask
    , toTaskBasic
    ) where

import Data.HashSet (HashSet)
import Data.Text (Text)
import Data.Time.Clock (UTCTime (..))
import Data.Time.LocalTime (TimeZone, utcToLocalTime)

import Data.HashSet qualified as HS
import Data.Text qualified as T

import Common.Interval (Extended (..), Interval, (<=..<=))
import Common.Prelude hiding (get, put)
import Domain.Core.Task.Raw
import Domain.Error

import Common.Interval qualified as I

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

nameLenBound :: Interval Int
nameLenBound = Finite 1 <=..<= Finite 30

memoLenBound :: Interval Int
memoLenBound = Finite 0 <=..<= Finite 60

tagSetLenBound :: Interval Int
tagSetLenBound = Finite 0 <=..<= Finite 4

tagLenBound :: Interval Int
tagLenBound = Finite 1 <=..<= Finite 10

mkTask :: TimeZone -> UTCTime -> EntryCreation -> Either DomainError Task
mkTask tz now EntryCreation{..} = do
    case deadline of
        EBound d -> validateDeadline tz now d
        EBoundless -> pure ()

    validateImportance importance
    validateName name
    validateMemo memo
    validateTagCount tags
    validateAllTagLength tags

    pure
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
modifyTask tz now entry task = do
    case entry.deadline of
        Just (EBound d) -> validateDeadline tz now d
        _ -> pure ()

    whenJust validateImportance entry.importance
    whenJust validateName entry.name
    whenJust validateMemo entry.memo
    whenJust validateTagCount entry.tags
    whenJust validateAllTagLength entry.tags

    pure
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

validateName :: Text -> Either DomainError ()
validateName = liftA2 ensure (InvalidNameLength nameLenBound) id . T.length
  where
    ensure :: DomainError -> Int -> Either DomainError ()
    ensure err = ensureInBound err nameLenBound

validateMemo :: Text -> Either DomainError ()
validateMemo = liftA2 ensure (InvalidMemoLength memoLenBound) id . T.length
  where
    ensure :: DomainError -> Int -> Either DomainError ()
    ensure err = ensureInBound err memoLenBound

validateTagCount :: HashSet Text -> Either DomainError ()
validateTagCount = liftA2 ensure (InvalidTagCount tagSetLenBound) id . length
  where
    ensure :: DomainError -> Int -> Either DomainError ()
    ensure err = ensureInBound err tagSetLenBound

validateAllTagLength :: HashSet Text -> Either DomainError ()
validateAllTagLength = (`HS.foldl'` Right ()) \acc tag ->
    acc
        *> ensureInBound
            (InvalidTagLength tagLenBound)
            tagLenBound
            (T.length tag)

whenJust :: (Applicative f) => (a -> f ()) -> Maybe a -> f ()
whenJust = maybe (pure ())

ensureInBound
    :: (Ord a) => DomainError -> Interval a -> a -> Either DomainError ()
ensureInBound err interval =
    bool (Left err) (Right ()) . (`I.member` interval)
