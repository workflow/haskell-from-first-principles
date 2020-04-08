import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Control.Applicative

newtype ZipList' a = ZipList' [a] deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let (ZipList' l) = xs
                    in take 3000 l
              ys' = let (ZipList' l) = ys
                    in take 3000 l

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
    pure a = ZipList' [a]
    _ <*> (ZipList' []) = ZipList' []
    (ZipList' []) <*> _ = ZipList' []
    (ZipList' (f:fs)) <*> (ZipList' (a:[])) = ZipList' $ (f a) : rest where
        (ZipList' rest) = (ZipList' fs <*> pure a)
    (ZipList' (f:[])) <*> (ZipList' (a:as)) = ZipList' $ (f a) : rest where
        (ZipList' rest) = (ZipList' $ pure f <*> as)
    (ZipList' (f:fs)) <*> (ZipList' (a:as)) = ZipList' $ (f a) : rest where
        (ZipList' rest) = (ZipList' fs <*> ZipList' as)

zl' = ZipList'
z = zl' [(+9), (*2), (+8)]
z' = zl' [1..3] 

tzls = ZipList' [("asd", "basd", "csd")]

instance Arbitrary a => Arbitrary (ZipList' a) where
    arbitrary = frequency [ (3, return $ ZipList' []), (1, ZipList' <$> arbitrary) ]