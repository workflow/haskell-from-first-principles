import Test.QuickCheck (quickCheck, Arbitrary, arbitrary, Gen, Function, CoArbitrary, Property, frequency)
import Data.Monoid
import Data.Semigroup

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

main :: IO ()
main = do
   quickCheck (semigroupAssoc :: TrivAssoc)
   quickCheck (semigroupAssoc :: IdentityAssoc)
   quickCheck (semigroupAssoc :: TwoAssoc)
   quickCheck (semigroupAssoc :: ThreeAssoc)
   quickCheck (semigroupAssoc :: BoolConjAssoc)
   quickCheck (semigroupAssoc :: BoolDisjAssoc)
   quickCheck (semigroupAssoc :: OrAssoc)
--    quickCheck $ \(Fn f) (Fn g) (Fn h) -> (combineAssoc :: CombineAssoc String String) (Combine f) (Combine g) (Combine h)
--    quickCheck (semigroupAssoc :: CompAssoc)
   let failure :: String -> Validation String Int
       failure = Failure
       success :: Int -> Validation String Int
       success = Success
   print $ success 1 <> failure "blah"
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

instance Semigroup (Identity a) where
    (Identity x) <> _ = Identity x

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

instance Semigroup (Two a b) where
    (Two a b) <> _ = Two a b
    
type TwoAssoc = Two Int String -> Two Int String -> Two Int String -> Bool

-- 4
data Three a b c = Three a b c deriving (Eq, Show)

genThree :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
genThree = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = genThree

instance Semigroup (Three a b c) where
    (Three a b c) <> _ = Three a b c
    
type ThreeAssoc = Three Int String Char -> Three Int String Char -> Three Int String Char -> Bool

-- 5
data Four a b c d = Four a b c d deriving (Eq, Show)

genFour :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
genFour = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    z' <- arbitrary
    return $ Four x y z z'

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = genFour

instance Semigroup (Four a b c d) where
    (Four a b c d) <> _ = Four a b c d
    
type FourAssoc = Four Int Int String Char -> Four Int Int String Char -> Four Int Int String Char -> Bool

-- 6
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

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 7
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

instance (Semigroup b) => Semigroup (Validation a b) where
    (Success a) <> _ = Success a
    (Failure b) <> (Success a) = Success a
    (Failure b) <> (Failure b') = Failure $ b <> b'

-- Why does it need a (Semigroup a) constraint?