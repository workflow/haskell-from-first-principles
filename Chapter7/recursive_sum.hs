module RecursiveSum where

recursiveSum :: (Eq a, Num a) => a -> a
recursiveSum 1 = 1
recursiveSum n = n + recursiveSum (n - 1)

