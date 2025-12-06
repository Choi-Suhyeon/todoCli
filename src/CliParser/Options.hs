module CliParser.Options
    ( Options (..)
    , Command (..)
    , AddCommand (..)
    , ListCommand (..)
    , EditCommand (..)
    , EditTags (..)
    , MarkCommand (..)
    , DeleteCommand (..)
    , ListStatus (..)
    , DeleteStatus (..)
    ) where

import Data.Char (isSpace)
import Data.HashSet (HashSet)
import Data.Text (Text)
import Data.Time.LocalTime (LocalTime)

import Data.Text qualified as T

import Common

data Options = Options
    { optCommand :: Command
    , verbose :: Bool
    }
    deriving (Generic, Show)

data Command
    = Add AddCommand
    | List ListCommand
    | Edit EditCommand
    | Mark MarkCommand
    | Delete DeleteCommand
    deriving (Generic, Show)

data AddCommand = AddCommand
    { name :: Text
    , deadline :: LocalTime
    , desc :: Text
    , tags :: HashSet Text
    }
    deriving (Generic, Show)

data ListCommand = ListCommand
    { tags :: HashSet Text
    , status :: Maybe ListStatus
    }
    deriving (Generic, Show)

data EditCommand = EditCommand
    { tgtName :: Text
    , name :: Maybe Text
    , desc :: Maybe Text
    , tags :: Maybe EditTags
    , deadline :: Maybe LocalTime
    }
    deriving (Generic, Show)

data EditTags = Clear | Substitute (HashSet Text)
    deriving (Generic, Show)

data MarkCommand
    = MrkDone Text
    | MrkUndone Text
    deriving (Generic, Show)

data DeleteCommand
    = DelAll
    | DelBy
        { byName :: Maybe Text
        , byTags :: Maybe (HashSet Text)
        , byStatus :: Maybe DeleteStatus
        }
    deriving (Generic, Show)

data ListStatus
    = LstDone
    | LstUndone
    | LstDue
    | LstOverdue
    deriving (Generic, Show)

instance Read ListStatus where
    readsPrec _ str = case T.toUpper tgt of
        "DONE" -> [(LstDone, restLtrimmed)]
        "UNDONE" -> [(LstUndone, restLtrimmed)]
        "DUE" -> [(LstDue, restLtrimmed)]
        "OVERDUE" -> [(LstOverdue, restLtrimmed)]
        _ -> []
      where
        (tgt, rest) = T.break isSpace . T.pack $ str
        restLtrimmed = T.unpack . T.stripStart $ rest

data DeleteStatus
    = DelDone
    | DelOverdue
    deriving (Generic, Show)

instance Read DeleteStatus where
    readsPrec _ str = case T.toUpper tgt of
        "DONE" -> [(DelDone, restLtrimmed)]
        "OVERDUE" -> [(DelOverdue, restLtrimmed)]
        _ -> []
      where
        (tgt, rest) = T.break isSpace . T.pack $ str
        restLtrimmed = T.unpack . T.stripStart $ rest
