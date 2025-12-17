module Common.Util ((.:), weave) where

import Common.Prelude

infixr 9 .:

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f .: g = (f .) . g

weave :: (Functor f1, Functor f2) => f1 (a -> b) -> f2 a -> f1 (f2 b)
weave fs xs = (<$> xs) <$> fs
