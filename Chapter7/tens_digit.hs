module TensDigit where

tensDigit :: Integral a => a -> a
--tensDigit x = d
--    where xLast = x `div` 10
--          d = xLast `mod` 10

tensDigit x = let
    (d', _) = x `divMod` 10
    (_, d) = d' `divMod` 10
    in d

hunsDigit x = let
    (d', _) = x `divMod` 100
    (_, d) = d' `divMod` 100
    in d
