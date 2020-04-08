import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Control.Applicative

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a la) = Cons (f a) (fmap f la)

instance Applicative List where
    pure a = Cons a Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons f lf) <*> as = (f <$> as) `append` (lf <*> as)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' (fmap f as)

instance Eq a => EqProp (List a) where
    (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = frequency [ (3, return Nil), (1, liftA2 Cons arbitrary arbitrary) ]

main :: IO ()
main = do
    let l = Cons ("asd", "bsd", "csd") Nil in
            quickBatch $ applicative l