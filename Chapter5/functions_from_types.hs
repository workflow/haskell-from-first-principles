module FunctionsFromTypes where

i :: a -> a
i x = x

c :: a -> b -> a
c x _ = x

c'' :: b -> a -> b
c'' x _ = x

c' :: a -> b -> b
c' _ x = x

r :: [a] -> [a]
r x = init x

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC $ aToB a

a :: (a -> c) -> a -> a
a _ x = x

a' :: (a -> b) -> a -> b
a' aToB a = aToB a