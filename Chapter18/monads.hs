import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative (liftA2)

main :: IO ()
main = do
    quickBatch $ functor nopeTrigger
    quickBatch $ applicative nopeTrigger
    quickBatch $ monad nopeTrigger

    quickBatch $ functor bahEitherTrigger
    quickBatch $ applicative bahEitherTrigger
    quickBatch $ monad bahEitherTrigger

    quickBatch $ functor identityTrigger
    quickBatch $ applicative identityTrigger
    quickBatch $ monad identityTrigger

    quickBatch $ functor listTrigger
    quickBatch $ applicative listTrigger
    quickBatch $ monad listTrigger

-- 1
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
    fmap f NopeDotJpg = NopeDotJpg

instance Applicative Nope where
    pure a = NopeDotJpg
    NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
    return = pure
    NopeDotJpg >>= f = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
    (=-=) = eq

nopeTrigger :: Nope (Int, String, Int)
nopeTrigger = undefined

-- 2
data BahEither b a = PLeft a | PRight b deriving (Eq, Show)

instance Functor (BahEither b) where
    fmap _ (PRight b) = PRight b
    fmap f (PLeft a) = PLeft $ f a

instance Applicative (BahEither b) where
    pure = PLeft 
    (PRight b) <*> _ = PRight b
    _ <*> (PRight b) = PRight b
    (PLeft f) <*> (PLeft a) = PLeft $ f a

instance Monad (BahEither b) where
    return = pure
    (PRight b) >>= _ = PRight b
    (PLeft a) >>= f = f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
    arbitrary = frequency [ (1, PLeft <$> arbitrary), (1, PRight <$> arbitrary) ]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
    (=-=) = eq

bahEitherTrigger :: BahEither (Int, String, Int) (Int, String, Int)
bahEitherTrigger = undefined

-- 3
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity a) = Identity $ f a

instance Monad Identity where
    return = pure
    (Identity a) >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

identityTrigger :: Identity (String, Int, String)
identityTrigger = undefined

-- 4
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a la) = Cons (f a) (f <$> la)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Applicative List where
    pure a = Cons a Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons f lf) <*> as = (f <$> as) `append` (lf <*> as)

instance Monad List where
    return = pure
    Nil >>= _ = Nil
    (Cons a la) >>= f = f a `append` (la >>= f)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = frequency [ (3, return Nil), (1, liftA2 Cons arbitrary arbitrary) ]

instance Eq a => EqProp (List a) where
    (=-=) = eq

listTrigger :: List (String, Int, String)
listTrigger = undefined