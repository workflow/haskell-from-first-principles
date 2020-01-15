module StdFuncs where

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem y (x:xs) = (x == y) || myElem y xs

myElemWithAny :: Eq a => a -> [a] -> Bool
myElemWithAny y xs = any (\x -> x == y) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = go f xs x
    where
        go _ (_:[]) gt = gt
        go f (x:xs) gt = if (f x gt) == GT then (go f xs x) else (go f xs gt)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = go f xs x
    where
        go _ (_:[]) lt = lt
        go f (x:xs) lt = if (f x lt) == LT then (go f xs x) else (go f xs lt)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMaximumBy compare
