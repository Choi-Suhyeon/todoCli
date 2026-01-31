module CliParser.Options
    ( Options (..)
    , Command (..)
    , AddCommand (..)
    , ListCommand (..)
    , EditCommand (..)
    , EditTags (..)
    , EditMemo (..)
    , OptionDeadline (..)
    , MarkCommand (..)
    , DeleteCommand (..)
    , ListStatus (..)
    , ListColumns (..)
    , DeleteStatus (..)
    ) where

import Data.HashSet (HashSet)
import Data.Text (Text)
import Data.Time.LocalTime (LocalTime)

import Data.Text qualified as T

import External.Interval (Interval)
import External.Prelude

data Options = Options
    { optCommand :: Command
    , verbose :: Bool
    }
    deriving (Show)

data Command
    = Add AddCommand
    | List ListCommand
    | Edit EditCommand
    | Mark MarkCommand
    | Delete DeleteCommand
    deriving (Show)

data AddCommand = AddCommand
    { name :: Text
    , deadline :: OptionDeadline
    , memo :: Text
    , tags :: HashSet Text
    , importance :: Word
    }
    deriving (Show)

data OptionDeadline = Boundless | Bound LocalTime
    deriving (Show)

data ListCommand = ListCommand
    { tags :: Maybe (HashSet Text)
    , status :: Maybe ListStatus
    , importance :: Maybe (Interval Word)
    , shouldReverse :: Bool
    , columns :: Maybe [ListColumns]
    }
    deriving (Show)

data EditCommand = EditCommand
    { tgtName :: Text
    , name :: Maybe Text
    , memo :: Maybe EditMemo
    , tags :: Maybe EditTags
    , deadline :: Maybe OptionDeadline
    , importance :: Maybe Word
    }
    deriving (Show)

data EditTags = Clear | Substitute (HashSet Text)
    deriving (Show)

data EditMemo = Remove | Memo Text
    deriving (Show)

data MarkCommand = MrkDone Text | MrkUndone Text
    deriving (Show)

data DeleteCommand
    = DelAll
    | DelBy
        { byName :: Maybe Text
        , byTags :: Maybe (HashSet Text)
        , byStatus :: Maybe DeleteStatus
        , byImportance :: Maybe (Interval Word)
        }
    deriving (Show)

data ListStatus = LstDone | LstUndone | LstDue | LstOverdue
    deriving (Show)

instance Read ListStatus where
    readsPrec _ str = case T.toUpper tgt of
        "DONE" -> [(LstDone, rest)]
        "UNDONE" -> [(LstUndone, rest)]
        "DUE" -> [(LstDue, rest)]
        "OVERDUE" -> [(LstOverdue, rest)]
        _ -> []
      where
        (tgt, rest) = splitFirstWord str

data ListColumns
    = LstName
    | LstMemo
    | LstTags
    | LstDeadline
    | LstStatus
    | LstImportance
    deriving (Show)

instance Read ListColumns where
    readsPrec _ str = case T.toUpper tgt of
        "NAME" -> [(LstName, rest)]
        "MEMO" -> [(LstMemo, rest)]
        "TAGS" -> [(LstTags, rest)]
        "DEADLINE" -> [(LstDeadline, rest)]
        "STATUS" -> [(LstStatus, rest)]
        "IMPORTANCE" -> [(LstImportance, rest)]
        _ -> []
      where
        (tgt, rest) = splitFirstWord str

data DeleteStatus
    = DelDone
    | DelOverdue
    deriving (Show)

instance Read DeleteStatus where
    readsPrec _ str = case T.toUpper tgt of
        "DONE" -> [(DelDone, rest)]
        "OVERDUE" -> [(DelOverdue, rest)]
        _ -> []
      where
        (tgt, rest) = splitFirstWord str

splitFirstWord :: String -> (Text, String)
splitFirstWord = second (into . T.stripStart) . T.break isSpace . into
