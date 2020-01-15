module Tuples where

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
--f x y = ((snd x, snd y), (fst x, fst y))
f (a, b) (c, d) = ((b, d), (a, c))