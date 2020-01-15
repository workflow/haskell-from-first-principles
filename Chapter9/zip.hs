module Zip where

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x, y) : Zip.zip xs ys

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = (f x y) : Zip.zipWith f xs ys

zipWithZipWith :: [a] -> [b] -> [(a, b)]
zipWithZipWith = Zip.zipWith (,)