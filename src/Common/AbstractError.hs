module Common.AbstractError
    ( throwErrorInto
    , liftEitherInto
    , liftEitherWith
    , liftEitherAs
    ) where

import External.Prelude

throwErrorInto :: forall e t m a. (From t e, MonadError e m) => t -> m a
throwErrorInto = throwError . into
{-# INLINE throwErrorInto #-}

liftEitherInto
    :: forall e t m a. (From t e, MonadError e m) => Either t a -> m a
liftEitherInto = either throwErrorInto pure
{-# INLINE liftEitherInto #-}

liftEitherWith
    :: forall e t b m a. (From t e, MonadError e m) => (b -> t) -> Either b a -> m a
liftEitherWith = (liftEitherInto .) . first
{-# INLINE liftEitherWith #-}

liftEitherAs
    :: forall e t b m a. (From t e, MonadError e m) => t -> Either b a -> m a
liftEitherAs = liftEitherWith . const
{-# INLINE liftEitherAs #-}
