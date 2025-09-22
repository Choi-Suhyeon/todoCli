module Domain.Error (MonadDomainError, DomainError(..)) where

import Data.Time.Format.ISO8601 (iso8601Show)
import Control.Monad.Except     (MonadError(..))
import Data.Time.LocalTime      (LocalTime)

import Common (FromErr)

type MonadDomainError e m = (MonadError e m, FromErr DomainError e)

data DomainError
    = EmptyTitle
    | TaskNotFound
    | DeserializationFailed String
    | InvalidDeadline LocalTime LocalTime

instance Show DomainError where
    show EmptyTitle                = "Title must not be empty"
    show TaskNotFound              = "The specified task could not be found"
    show (DeserializationFailed s) = "Deserialization failed: " <> s
    show (InvalidDeadline c v)     = "Deadline must be in the future (current: " <> iso8601Show c <> ", input: " <> iso8601Show v <> ")"

