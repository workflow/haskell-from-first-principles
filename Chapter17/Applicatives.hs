
import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Control.Applicative

main :: IO ()
main = do
    putStr "Pair"
    quickBatch $ applicative pt

    putStr "Two"
    quickBatch $ applicative tt

    putStr "Three"
    quickBatch $ applicative tht

    putStr "Three'"
    quickBatch $ applicative thpt

    putStr "Four"
    quickBatch $ applicative ft

    putStr "Four'"
    quickBatch $ applicative fpt

-- 1
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
    pure a = Pair a a
    (Pair f g) <*> (Pair a a') = Pair (f a) (g a')

pt = Pair ("asd", "bsd", "csd") ("fu", "bar", "baz")

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = liftA2 Pair arbitrary arbitrary

instance (Eq a) => EqProp (Pair a) where
    (=-=) = eq

-- 2
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (Monoid a) => Applicative (Two a) where
    pure b = Two mempty b
    (Two a f) <*> (Two a' b) = Two (a <> a') (f b)

tt = Two "whatever" ("asd", "bsd", "csd")

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = liftA2 Two arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq

-- 3
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure c = Three mempty mempty c
    (Three a b f) <*> (Three a' b' c) = Three (a <> a') (b <> b') (f c)

tht = Three "what" "ever" ("asd", "bsd", "csd")

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

-- 4
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Monoid a) => Applicative (Three' a) where
    pure c = Three' mempty c c
    (Three' a f g) <*> (Three' a' b b') = Three' (a <> a') (f b) (g b')

thpt = Three' "what" ("fu", "bar", "baz") ("asd", "bsd", "csd")

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = liftA3 Three' arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq

-- 4
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure d = Four mempty mempty mempty d
    (Four a b c f) <*> (Four a' b' c' d) = Four (a <> a') (b <> b') (c <> c') (f d)

ft = Four "what" "ev" "er" ("asd", "bsd", "csd")

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) = eq

-- 5
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Monoid a) => Applicative (Four' a) where
    pure b = Four' mempty mempty mempty b
    (Four' a a1 a2 f) <*> (Four' a' a1' a2' b) = Four' (a <> a') (a1 <> a1') (a2 <> a2') (f b)

fpt = Four' "what" "ev" "er" ("asd", "bsd", "csd")

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq
