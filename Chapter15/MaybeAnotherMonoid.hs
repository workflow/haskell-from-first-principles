
import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Semigroup (Optional a) where
    (<>) Nada Nada = Nada
    (<>) Nada (Only x) = Only x
    (<>) (Only x) Nada = Only x
    (<>) (Only x) (Only y) = Only (x <> y)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada Nada = Nada
  mappend Nada (Only x) = Only x
  mappend (Only x) Nada = Only x
  mappend (Only x) (Only y) = Only (x `mappend` y)

genOnly :: Arbitrary a => Gen (Optional a)
genOnly = do
  x <- arbitrary
  return $ Only x

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary =
    frequency [ (1, genOnly)
              , (1, return Nada) ]

newtype First' a = 
    First' { getFirst' :: Optional a } deriving (Eq, Show)

genFirst' :: Arbitrary a => Gen (First' a)
genFirst' = do
    x <- arbitrary
    return $ First' x

instance Arbitrary a => Arbitrary (First' a) where 
    arbitrary = genFirst'

instance Semigroup (First' a) where
    (<>) (First' {getFirst' = Nada}) y = y
    (<>) (First' {getFirst' = Only x}) _ = First' $ Only x

instance Monoid (First' a) where
    mempty = First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
    First' String
    -> First' String
    -> First' String
    -> Bool

type FstId = First' String -> Bool

main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)
