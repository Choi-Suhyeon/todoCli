module Domain.Type (module Domain.Type.Internal, MonadRegistry) where

import Control.Monad.State.Strict (MonadState)

import Domain.Type.Internal (TodoRegistry, initTodoRegistry)

type MonadRegistry m = MonadState TodoRegistry m
