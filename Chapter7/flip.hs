module Flip where

flipflop :: (a -> b -> c) -> b -> a -> c
--flipflop f x y = f y x
flipflop f = \ x y -> f y x