module Domain.Error (MonadDomainError, DomainError (..)) where

import Control.Monad.Except (MonadError (..))
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (LocalTime)
import Text.Printf (printf)
import Witch

type MonadDomainError e m = (MonadError e m, From DomainError e)

data DomainError
    = EmptyTitle
    | TaskNotFound
    | DeserializationFailed String
    | InvalidDeadline LocalTime LocalTime
    | TaskIdExhausted

instance Show DomainError where
    show EmptyTitle = "Title must not be empty"
    show TaskNotFound = "The specified task could not be found"
    show (DeserializationFailed s) = "Deserialization failed: " <> s
    show (InvalidDeadline c v) =
        printf
            "Deadline must be in the future (current: %s, input: %s)"
            (iso8601Show c)
            (iso8601Show v)
    show TaskIdExhausted = "No available task ID: maximum capacity reached"
