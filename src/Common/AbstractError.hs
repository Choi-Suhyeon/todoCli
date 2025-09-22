module Common.AbstractError (FromErr(..), throwErrorFrom, liftEitherFrom, liftEitherWith, liftEitherAs) where

import Control.Monad.Except (MonadError(..))
import Data.Bifunctor       (first)

class FromErr t e | t -> e where
    fromErr :: t -> e

throwErrorFrom :: forall e t m a. (MonadError e m, FromErr t e) => t -> m a
throwErrorFrom = throwError . fromErr
{-# INLINE throwErrorFrom #-}

liftEitherFrom :: forall e t m a. (MonadError e m, FromErr t e) => Either t a -> m a 
liftEitherFrom = either throwErrorFrom pure
{-# INLINE liftEitherFrom #-}

liftEitherWith :: forall e t b m a. (MonadError e m, FromErr t e) => (b -> t) -> Either b a -> m a 
liftEitherWith = (liftEitherFrom .) . first
{-# INLINE liftEitherWith #-}

liftEitherAs :: forall e t b m a. (MonadError e m, FromErr t e) => t -> Either b a -> m a
liftEitherAs = liftEitherWith . const
{-# INLINE liftEitherAs #-}

