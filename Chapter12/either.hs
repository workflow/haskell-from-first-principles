isLeft :: Either a b -> Bool
isLeft (Left x) = True
isLeft (Right y) = False 

isRight :: Either a b -> Bool
isRight x = not $ isLeft x

unwrapLeft :: Either a b -> a
unwrapLeft (Left a) = a

unwrapRight :: Either a b -> b
unwrapRight (Right b) = b

lefts' :: [Either a b] -> [a]
lefts' = foldr (\x acc -> if isLeft x then unwrapLeft x : acc else acc) [] 

rights' :: [Either a b] -> [b]
rights' = foldr (\x acc -> if isRight x then unwrapRight x : acc else acc) [] 

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just $ f b
eitherMaybe' f (Left _) = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\x -> Nothing) (\x -> Just $ f x)
