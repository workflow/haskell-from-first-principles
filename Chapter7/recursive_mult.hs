module RecursiveMult where

recursiveMult :: (Integral a) => a -> a -> a
recursiveMult x y = go x y y where
    go x y 1 = x
    go x y count = x + go x y (count - 1)