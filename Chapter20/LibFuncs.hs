import Data.Monoid
import Data.Foldable (foldMap)

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

elem :: (Foldable t, Eq a) => a -> t a -> Bool
-- elem x = getAny . foldMap (Any . (== x))
elem = (getAny.) . foldMap . (Any.) . (==)

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr f Nothing where
    f a (Just x) = Just $ min x a
    f a Nothing = Just a 

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr f Nothing where
    f a (Just x) = Just $ max x a
    f a Nothing = Just a 

null :: (Foldable t) => t a -> Bool
null = foldr f True
    where f _ _ = False

length :: (Foldable t) => t a -> Int 
-- length = foldr f 0 where
    -- f _ acc = acc + 1
length = getSum . foldMap (const $ Sum 1)

toList :: (Foldable t) => t a -> [a]
-- toList = foldr (:) []
toList = foldMap pure

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr g mempty where 
    g a m = f a <> m
    