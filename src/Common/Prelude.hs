module Common.Prelude
    ( module Prelude
    , module Control.Applicative
    , module Control.Monad
    , module Control.Monad.Except
    , module Control.Monad.Reader
    , module Control.Monad.State.Strict
    , module Control.Monad.Writer.Strict
    , module Data.Bifunctor
    , module Data.Bool
    , module Data.Char
    , module Data.Either
    , module Data.Foldable
    , module Data.Function
    , module Data.Functor
    , module Data.Maybe
    , module Data.Monoid
    , module Data.Semigroup
    , module Data.String
    , module Data.Traversable
    , module Witch
    , maybeToEither
    , tee
    , teeM
    , (>!)
    ) where

import "base" Prelude hiding (unzip)

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Either
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.Semigroup hiding (First (..), Last (..))
import Data.String
import Data.Traversable
import Witch

infixl 1 >!

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing = Left a

tee :: (Functor f) => (a -> f b) -> a -> f a
tee = liftA2 (<$) id

teeM :: (Monad m) => (a -> m b) -> m a -> m a
teeM = (=<<) . tee

(>!) :: (Monad m) => m a -> (a -> m b) -> m a
(>!) = flip teeM
