module External.Interval
    ( module Data.Interval
    , fromFinite
    , show'
    , upperIntegralBound
    , lowerIntegralBound
    ) where

import Data.Interval

import External.Prelude

fromFinite :: Extended r -> Maybe r
fromFinite (Finite n) = Just n
fromFinite _ = Nothing

show' :: (Show r) => Extended r -> String
show' (Finite n) = show n
show' PosInf = "infinity"
show' NegInf = "negative infinity"

upperIntegralBound :: (Integral r) => Interval r -> Extended r
upperIntegralBound = flip calcIntegralBound upperBound' \case
    Closed -> id
    Open -> fmap pred

lowerIntegralBound :: (Integral r) => Interval r -> Extended r
lowerIntegralBound = flip calcIntegralBound lowerBound' \case
    Closed -> id
    Open -> fmap succ

calcIntegralBound
    :: (Boundary -> Extended r -> Extended r)
    -> (Interval r -> (Extended r, Boundary))
    -> Interval r
    -> Extended r
calcIntegralBound adjust getBound =
    uncurry (&) . second adjust . getBound
