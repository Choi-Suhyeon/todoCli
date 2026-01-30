module Domain.Error (MonadDomainError, DomainError (..)) where

import Data.Time.LocalTime (LocalTime)
import Formatting (Format, formatToString, int, string, (%), (%+))

import Common
import External.ISO8601 (iso8601ShowNoFrac)
import External.Interval (Interval)
import External.Prelude

import External.Interval qualified as I

type MonadDomainError e m = MonadErrorFrom DomainError e m

data DomainError
    = EmptyTitle
    | TaskNotFound
    | InvalidDeadline LocalTime LocalTime
    | TaskIdExhausted
    | ImportanceOutOfRange
    | InvalidNameLength (Interval Int) Int
    | InvalidMemoLength (Interval Int) Int
    | InvalidTagCount (Interval Int) Int
    | InvalidTagLength (Interval Int)

instance Show DomainError where
    show EmptyTitle = "Title must not be empty"
    show TaskNotFound = "The specified task could not be found"
    show TaskIdExhausted = "No available task ID: maximum capacity reached"
    show ImportanceOutOfRange = "Importance value is out of valid range"
    show (InvalidNameLength i l) = lengthOutOfRange "Name length" i l
    show (InvalidMemoLength i l) = lengthOutOfRange "Memo length" i l
    show (InvalidTagCount i l) = lengthOutOfRange "Tag count" i l
    show (InvalidTagLength i) = simpleLengthOutOfRange "Tag length" i
    show (InvalidDeadline c v) = (formatToString formatStr `on` iso8601ShowNoFrac) c v
      where
        formatStr :: Format String (String -> String -> String)
        formatStr = baseMsg %+ detailsFmt
          where
            baseMsg = "Deadline must be in the future"
            detailsFmt = "(current:" %+ string % ", input:" %+ string % ")"

lengthOutOfRange :: String -> Interval Int -> Int -> String
lengthOutOfRange elemName interval len =
    formatToString
        (string %+ ", but got" %+ int)
        (simpleLengthOutOfRange elemName interval)
        len

simpleLengthOutOfRange :: String -> Interval Int -> String
simpleLengthOutOfRange elemName interval =
    formatToString
        (string %+ "must be between" %+ string %+ "and" %+ string)
        elemName
        (I.show' $ I.lowerBound interval)
        (I.show' $ I.upperBound interval)
