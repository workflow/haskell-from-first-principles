import Test.QuickCheck
import Data.List (sort)

prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse ys ++ reverse xs

half x = x / 2

halfIdentity = (*2) . half

prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = x == halfIdentity x

-- for any list you apply sort to,
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t) = (Just y, t)
          go y (Just x, t) = (Just y, x >= y)


plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative x y = x + y == y + x

-- 4

timesAssociative x y z = x * (y * z) == (x * y) * z

timesCommutative x y = x * y == y * x

-- 5

prop_quotRem :: Int -> Int -> Bool
prop_quotRem _ 0 = True
prop_quotRem x y = (quot x y) * y + (rem x y) == x

prop_divMod :: Int -> Int -> Bool
prop_divMod _ 0 = True
prop_divMod x y = (div x y) * y + (mod x y) == x

-- 6

prop_caretAssociative :: Int -> Int -> Int -> Bool
prop_caretAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

prop_caretCommutative :: Int -> Int -> Bool
prop_caretCommutative x y = x ^ y == y ^ x

-- 7
prop_reverseListTwice :: (Eq a) => [a] -> Bool
prop_reverseListTwice xs = (reverse . reverse) xs == xs

-- 8
prop_dollar :: (Eq b) => (a -> b) -> a -> Bool
prop_dollar f x = (f $ x) == (f x)