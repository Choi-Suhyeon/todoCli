module CliParser.Options 
    ( Options(..)       , Command(..)       , AddCommand(..)
    , ListCommand(..)   , EditCommand(..)   , MarkCommand(..)
    , DeleteCommand(..) , ListStatus(..)    , DeleteStatus(..)
    ) where

import Data.Text    qualified as T

import Data.Time.LocalTime (LocalTime)
import GHC.Generics        (Generic)
import Data.HashSet        (HashSet)
import Data.Text           (Text)
import Data.Char           (isSpace)

data Options = Options { optCommand :: Command }
  deriving (Show, Generic)

data Command
    = Add    AddCommand
    | List   ListCommand
    | Edit   EditCommand
    | Mark   MarkCommand
    | Delete DeleteCommand
  deriving (Show, Generic)

data AddCommand
    = AddCommand
    { name     :: Text
    , deadline :: LocalTime
    , desc     :: Text
    , tags     :: HashSet Text
    }
  deriving (Show, Generic)

data ListCommand
    = ListCommand
    { tags   :: HashSet Text
    , status :: Maybe ListStatus
    }
  deriving (Show, Generic)

data EditCommand
    = EditCommand
    { tgtName  :: Text
    , name     :: Maybe Text
    , desc     :: Maybe Text
    , tags     :: Maybe (HashSet Text)
    , deadline :: Maybe LocalTime
    }
  deriving (Show, Generic)

data MarkCommand
    = MrkDone   Text
    | MrkUndone Text
  deriving (Show, Generic)

data DeleteCommand
    = DelAll
    | DelBy
        { byName   :: Maybe Text
        , byTags   :: Maybe (HashSet Text)
        , byStatus :: Maybe DeleteStatus
        }
  deriving (Show, Generic)

data ListStatus 
    = LstDone 
    | LstUndone 
    | LstDue 
    | LstOverdue
  deriving (Show, Generic)

instance Read ListStatus where
    readsPrec _ str = case T.toUpper tgt of
        "DONE"    -> [(LstDone, restLtrimmed)]
        "UNDONE"  -> [(LstUndone, restLtrimmed)]
        "DUE"     -> [(LstDue, restLtrimmed)]
        "OVERDUE" -> [(LstOverdue, restLtrimmed)]
        _         -> []
      where
        (tgt, rest)  = T.break isSpace . T.pack $ str
        restLtrimmed = T.unpack . T.stripStart $ rest

data DeleteStatus
    = DelDone
    | DelOverdue
  deriving (Show, Generic)

instance Read DeleteStatus where
    readsPrec _ str = case T.toUpper tgt of
        "DONE"    -> [(DelDone, restLtrimmed)]
        "OVERDUE" -> [(DelOverdue, restLtrimmed)]
        _         -> []
      where
        (tgt, rest)  = T.break isSpace . T.pack $ str
        restLtrimmed = T.unpack . T.stripStart $ rest
