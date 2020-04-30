
import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Control.Applicative

main :: IO ()
main = do
    quickBatch $ traversable identityTrigger
    quickBatch $ traversable constantTrigger
    quickBatch $ traversable optionalTrigger
    quickBatch $ traversable listTrigger
    quickBatch $ traversable threeTrigger
    quickBatch $ traversable pairTrigger
    quickBatch $ traversable bigTrigger
    quickBatch $ traversable biggerTrigger
    quickBatch $ traversable treeTrigger

-- Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity $ f x

instance Foldable Identity where
    foldMap f (Identity x) = f x

instance Traversable Identity where
    traverse f (Identity x) = Identity <$> f x 

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

identityTrigger :: Identity (String, Int, String)
identityTrigger = undefined

-- Constant
newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Show)

instance Functor (Constant a) where
    fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
    foldMap f (Constant x) = mempty

instance Traversable (Constant a) where
    traverse f (Constant a) = Constant <$> pure a 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
    arbitrary = fmap Constant arbitrary

instance (Eq a, Eq b) => EqProp (Constant a b) where
    (=-=) = eq

constantTrigger :: Constant (String, Int, String) (String, Int, String)
constantTrigger = undefined

-- Maybe
data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
    foldMap _ Nada = mempty
    foldMap f (Yep a) = f a

instance Traversable Optional where
    traverse _ Nada = pure Nada
    traverse f (Yep a) = Yep <$> f a

instance (Arbitrary a) => Arbitrary (Optional a) where
    arbitrary = frequency [ (3, return Nada), (1, Yep <$> arbitrary)]

instance Eq a => EqProp (Optional a) where
    (=-=) = eq

optionalTrigger :: Optional (String, Int, String)
optionalTrigger = undefined

-- List
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a la) = Cons (f a) (fmap f la)

instance Foldable List where
    foldMap _ Nil = mempty 
    foldMap f (Cons a la) = f a <> foldMap f la

instance Traversable List where
    traverse _ Nil = pure Nil
    traverse f (Cons a la) = liftA2 Cons (f a) (traverse f la)

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = frequency [ (3, return Nil), (1, liftA2 Cons arbitrary arbitrary)]

instance Eq a => EqProp (List a) where
    (=-=) = eq

listTrigger :: List (String, Int, String)
listTrigger = undefined

-- Three
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
    foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
    traverse f (Three a b c) = Three a b <$> (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

threeTrigger :: Three (String, String, String) (String, String, String) (String, String, String)
threeTrigger = undefined

-- Pair
data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
    foldMap f (Pair _ b) = f b

instance Traversable (Pair a) where
    traverse f (Pair a b) = Pair a <$> (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = liftA2 Pair arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
    (=-=) = eq

pairTrigger :: Pair (String, String, String) (String, String, String)
pairTrigger = undefined

-- Big
data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
    fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
    foldMap f (Big a b b') = f b <> f b'

instance Traversable (Big a) where
    traverse f (Big a b b') = Big a <$> (f b) <*> (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
    arbitrary = liftA3 Big arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
    (=-=) = eq

bigTrigger :: Big (String, String, String) (String, String, String)
bigTrigger = undefined

-- Bigger
data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
    fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
    foldMap f (Bigger a b b' b'') = f b <> f b' <> f b''

instance Traversable (Bigger a) where
    traverse f (Bigger a b b' b'') = Bigger a <$> (f b) <*> (f b') <*> (f b'')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
    arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
    (=-=) = eq

biggerTrigger :: Bigger (String, String, String) (String, String, String)
biggerTrigger = undefined

-- S -> see S.hs

-- Tree
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf a) = Leaf $ f a
    fmap f (Node t1 a t2) = Node (fmap f t1) (f a) (fmap f t2)

instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node t1 a t2) = (foldMap f t1) <> f a <> (foldMap f t2)

instance Traversable Tree where
    traverse _ Empty = pure Empty
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Node t1 a t2) = Node <$> (traverse f t1) <*> f a <*> (traverse f t2)

instance (Arbitrary a) => Arbitrary (Tree a) where
    arbitrary = frequency [
        (3, return Empty),
        (3, fmap Leaf arbitrary),
        (1, liftA3 Node arbitrary arbitrary arbitrary)
        ]

instance (Eq a) => EqProp (Tree a) where
    (=-=) = eq

treeTrigger :: Tree (String, String, String)
treeTrigger = undefined