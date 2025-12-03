module Common.Util (tee, teeM, (>!), (.:), weave) where

infixl 1 >!
infixr 9 .:

tee :: (Functor f) => (a -> f b) -> a -> f a
tee = liftA2 (<$) id

teeM :: (Monad m) => (a -> m b) -> m a -> m a
teeM = (=<<) . tee

(>!) :: (Monad m) => m a -> (a -> m b) -> m a
(>!) = flip teeM

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f .: g = (f .) . g

weave :: (Functor f1, Functor f2) => f1 (a -> b) -> f2 a -> f1 (f2 b)
weave fs xs = (<$> xs) <$> fs
