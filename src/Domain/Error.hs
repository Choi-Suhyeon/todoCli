module Domain.Error (ErrCode(..), Result) where

import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (LocalTime)

data ErrCode
    = EmptyTitle
    | TaskNotFound
    | DeserializationFailed String
    | InvalidDeadline LocalTime LocalTime

instance Show ErrCode where
    show EmptyTitle                = "Title must not be empty"
    show TaskNotFound              = "The specified task could not be found"
    show (DeserializationFailed s) = "Deserialization failed: " <> s <> ")"
    show (InvalidDeadline c v)     = "Deadline must be in the future (current: " <> iso8601Show c <> ", input: " <> iso8601Show v <> ")"

type Result a = Either ErrCode a

