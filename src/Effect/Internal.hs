module Effect.Internal (tee, teeM, (>!)) where

infixl 1 >!

tee :: Functor f => (a -> f b) -> a -> f a
tee = liftA2 (<$) id

teeM :: Monad m => (a -> m b) -> m a -> m a
teeM = (=<<) . tee

(>!) :: Monad m => m a -> (a -> m b) -> m a
(>!) = flip teeM
