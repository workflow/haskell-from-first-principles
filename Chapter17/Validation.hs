
import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Control.Applicative

data Validation e a = Failure e | Success a deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success $ f a

instance Monoid e => Applicative (Validation e) where
    pure a = Success a
    (Failure e) <*> (Failure e') = Failure (e <> e')
    (Failure e) <*> _ = Failure e
    _ <*> (Failure e) = Failure e
    (Success f) <*> (Success a) = Success (f a)

tv = Success ("asd", "basd", "fubarbaz")

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
    arbitrary = frequency [ (1, Failure <$> arbitrary), (1, Success <$> arbitrary) ]

instance (Eq a, Eq e) => EqProp (Validation e a) where
    (=-=) = eq