import Test.QuickCheck (quickCheck, Arbitrary, arbitrary, Gen, Function, CoArbitrary, Property, frequency)
import Data.Monoid
import Data.Semigroup

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

instance Arbitrary Trivial where
    arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

main :: IO ()
main = do
   quickCheck (semigroupAssoc :: TrivAssoc)
   quickCheck (monoidLeftIdentity :: Trivial -> Bool)
   quickCheck (monoidRightIdentity :: Trivial -> Bool)

   print "Identity"
   quickCheck (semigroupAssoc :: IdentityAssoc)
   quickCheck (monoidLeftIdentity :: Identity String -> Bool)
   quickCheck (monoidRightIdentity :: Identity String -> Bool)

   print "Two"
   quickCheck (semigroupAssoc :: TwoAssoc)
   quickCheck (monoidLeftIdentity :: Two String String -> Bool)
   quickCheck (monoidRightIdentity :: Two String String -> Bool)

   print "BoolConj"
   quickCheck (semigroupAssoc :: BoolConjAssoc)
   quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
   quickCheck (monoidRightIdentity :: BoolConj -> Bool)

   print "BoolDisj"
   quickCheck (semigroupAssoc :: BoolDisjAssoc)
   quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
   quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

   quickCheck (semigroupAssoc :: OrAssoc)
--    quickCheck $ \(Fn f) (Fn g) (Fn h) -> (combineAssoc :: CombineAssoc String String) (Combine f) (Combine g) (Combine h)
--    quickCheck (semigroupAssoc :: CompAssoc)
--    let failure :: String -> Validation String Int
--        failure = Failure
--        success :: Int -> Validation String Int
--        success = Success
--    print $ success 1 <> failure "blah"
--    print $ failure "woot" <> failure "blah"
--    print $ success 1 <> success 2
--    print $ failure "woot" <> success 2


newtype Identity a = Identity a deriving (Eq, Show)

genIdentity :: Arbitrary a => Gen (Identity a)
genIdentity = do
    x <- arbitrary
    return $ Identity x

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = genIdentity

instance Semigroup a => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity (x <> y)

instance (Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

-- 3
data Two a b = Two a b deriving (Eq, Show)

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = genTwo

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    
type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

-- 4
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

genBoolConj :: Gen BoolConj
genBoolConj = do
    x <- arbitrary
    return $ BoolConj x

instance Arbitrary BoolConj where
    arbitrary = genBoolConj

instance Semigroup BoolConj where
    (BoolConj False) <> _ = BoolConj False 
    (BoolConj True) <> (BoolConj False) = BoolConj False 
    (BoolConj True) <> (BoolConj True) = BoolConj True 

instance Monoid BoolConj where
    mempty = BoolConj True

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 5
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

genBoolDisj :: Gen BoolDisj
genBoolDisj = do
    x <- arbitrary
    return $ BoolDisj x

instance Arbitrary BoolDisj where
    arbitrary = genBoolDisj

instance Semigroup BoolDisj where
    (BoolDisj True) <> _ = BoolDisj True 
    (BoolDisj False) <> (BoolDisj True) = BoolDisj True
    (BoolDisj False) <> (BoolDisj False) = BoolDisj False 

instance Monoid BoolDisj where
    mempty = BoolDisj False

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8
data Or a b = Fst a | Snd b deriving (Eq, Show)

genFst :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
genFst = do
    x <- arbitrary
    return $ Fst x

genSnd :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
genSnd = do
    x <- arbitrary
    return $ Snd x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = frequency [ (1, genFst), (1, genSnd) ]

instance Semigroup (Or a b) where
    (Fst a) <> (Fst a') = Fst a'
    (Fst a) <> (Snd b) = Snd b
    (Snd b) <> _ = Snd b

type OrAssoc = Or String Int -> Or String Int -> Or String Int -> Bool

-- 9
newtype Combine a b = Combine { unCombine :: (a -> b)}

-- instance (Eq a, Eq b) => Eq (Combine a b) where
--     (==) (Combine f) (Combine g) = (f == g)

-- genCombine :: (Arbitrary a, Arbitrary b, Semigroup a, Semigroup b, CoArbitrary a) => Gen (Combine a b)
-- genCombine = do
--     f <- arbitrary
--     return $ Combine f

-- instance (Arbitrary a, Arbitrary b, Semigroup a, Semigroup b, CoArbitrary a) => Arbitrary (Combine a b) where
--     arbitrary = genCombine

-- From https://stackoverflow.com/questions/41350192/how-to-test-semigroup-law-for-this-data-type
-- Test.QuickCheck.(===) requires (Eq b, Show b)
-- but you can use (==) if you prefer.
-- funcEquality :: (Arbitrary a, Show a, Eq b, Show b, Semigroup b) => Combine a b -> Combine a b -> Property
-- funcEquality (Combine f) (Combine g) = property $ \a -> f a === g a


-- instance (Semigroup a, Semigroup b) => Semigroup (Combine a b) where
--     (Combine f) <> (Combine g) = Combine (\x -> f x <> g x)

-- type CombineAssoc a b = Combine a b -> Combine a b -> Combine a b -> Property

-- combineAssoc :: (Arbitrary a, Show a, Semigroup a, Eq b, Show b, Semigroup b) => CombineAssoc a b
-- combineAssoc f g h = ((f <> g) <> h) `funcEquality` (f <> (g <> h))

-- 10
newtype Comp a = Comp { unComp :: (a -> a) }

instance Show (a -> b) where
    show a = "Function"

genComp :: (Arbitrary a, CoArbitrary a)  => Gen (Comp a)
genComp = do
    f <- arbitrary
    return $ Comp f

instance (Arbitrary a, CoArbitrary a) => Arbitrary (Comp a) where
    arbitrary = genComp

instance Semigroup (Comp a) where
    (Comp f) <> (Comp g) = Comp $ f . g

type CompAssoc = Comp String -> Comp String -> Comp String -> Bool

-- 11
data Validation a b = Failure a | Success b deriving (Eq, Show)

-- instance (Semigroup b) => Semigroup (Validation a b) where
--     (Success a) <> _ = Success a
--     (Failure b) <> (Success a) = Success a
--     (Failure b) <> (Failure b') = Failure $ b <> b'

-- Why does it need a (Semigroup a) constraint?