module Common.AbstractError
    ( IsError (..)
    , FromErr (..)
    , throwErrorFrom
    , liftEitherFrom
    , liftEitherWith
    , liftEitherAs
    ) where

import Control.Monad.Except (MonadError (..))
import Data.Bifunctor (first)

class IsError e where
    displayError :: e -> String

class (IsError e) => FromErr t e | t -> e where
    fromErr :: t -> e

throwErrorFrom :: forall e t m a. (FromErr t e, MonadError e m) => t -> m a
throwErrorFrom = throwError . fromErr
{-# INLINE throwErrorFrom #-}

liftEitherFrom
    :: forall e t m a. (FromErr t e, MonadError e m) => Either t a -> m a
liftEitherFrom = either throwErrorFrom pure
{-# INLINE liftEitherFrom #-}

liftEitherWith
    :: forall e t b m a. (FromErr t e, MonadError e m) => (b -> t) -> Either b a -> m a
liftEitherWith = (liftEitherFrom .) . first
{-# INLINE liftEitherWith #-}

liftEitherAs
    :: forall e t b m a. (FromErr t e, MonadError e m) => t -> Either b a -> m a
liftEitherAs = liftEitherWith . const
{-# INLINE liftEitherAs #-}
