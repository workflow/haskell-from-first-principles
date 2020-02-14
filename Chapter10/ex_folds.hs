myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myOr' :: [Bool] -> Bool
myOr' = foldr (\x y -> x || y) False

myOr'' :: [Bool] -> Bool
myOr'' = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = f x || myAny f xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = foldr (\x y -> f x || y) False

myAny'' :: (a -> Bool) -> [a] -> Bool
myAny'' f = foldr ((||) . f) False

--myAny''' :: (a -> Bool) -> [a] -> Bool
--myAny''' = (flip foldr) False $ (.) (||)

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\a acc -> a == x) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny' (== x)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = (last xs) : (myReverse $ init xs)

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f as = foldr (\x y -> f x : y) [] as

myMap' :: (a -> b) -> [a] -> [b]
myMap' f as = foldr ((:) . f) [] as

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x y -> if f x then x : y else y) []

squish :: [[a]] -> [a]
squish = foldr (\x y -> foldr (\a b -> a : b) y x) []

squish' :: [[a]] -> [a]
squish' = foldr (flip $ foldr (:)) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x y -> foldr (\a b -> a : b) y (f x)) []

squishMap' :: (a -> [b]) -> [a] -> [b]
squishMap' f = foldr ((flip $ foldr (:) ) . f $) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = go f xs x where
    go _ (_:[]) gt = gt
    go f (x:xs) gt = if f x gt == GT then go f xs x else go f xs gt

myMaximumBy' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy' f xs = foldr (\x gt -> if f x gt == GT then x else gt) (last xs) xs

myMinimumBy' :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy' f xs = foldr (\x gt -> if f x gt == LT then x else gt) (last xs) xs
