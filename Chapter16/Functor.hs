import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
functorCompose' (Fun _ f) (Fun _ g) x = (fmap g (fmap f x)) == (fmap (g . f) x)

main :: IO ()
main = do
   quickCheck (functorIdentity :: Identity Int -> Bool) 
   quickCheck (functorCompose' :: IdentityCompose) 

   quickCheck (functorIdentity :: Pair Int -> Bool)
   quickCheck (functorCompose' :: PairCompose)

   quickCheck (functorIdentity :: Two Int String -> Bool)
   quickCheck (functorCompose' :: TwoCompose)

   quickCheck (functorIdentity :: Three Char Int String -> Bool)
   quickCheck (functorCompose' :: ThreeCompose)

   quickCheck (functorIdentity :: Three' Char Int-> Bool)
   quickCheck (functorCompose' :: Three'Compose)

   quickCheck (functorIdentity :: Four Char Int Bool String -> Bool)
   quickCheck (functorCompose' :: FourCompose)

   quickCheck (functorIdentity :: Four' Char Int -> Bool)
   quickCheck (functorCompose' :: Four'Compose)

-- 1
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity $ f x

genIdentity :: Arbitrary a => Gen (Identity a)
genIdentity = do
    x <- arbitrary
    return $ Identity x

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = genIdentity

type IdentityCompose = Fun String String -> Fun String String -> Identity String -> Bool

-- 2
data Pair a = Pair a a deriving (Eq, Show)

genPair :: Arbitrary a => Gen (Pair a)
genPair = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = genPair

type PairCompose = Fun String String -> Fun String String -> Pair String -> Bool

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)


-- 3
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = genTwo

type TwoCompose = Fun String String -> Fun String String -> Two String String -> Bool

-- 4
data Three a b c = Three a b c deriving ( Eq, Show )

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

genThree :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
genThree = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = genThree

type ThreeCompose = Fun String String -> Fun String String -> Three String String String -> Bool

-- 5
data Three' a b = Three' a b b deriving ( Eq, Show )

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

genThree' :: (Arbitrary a, Arbitrary b) => Gen (Three' a b)
genThree' = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three' x y z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = genThree'

type Three'Compose = Fun String String -> Fun String String -> Three' String String -> Bool

-- 6
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

genFour :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
genFour = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four w x y z

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = genFour


type FourCompose = Fun String String -> Fun String String -> Four String String String String -> Bool

-- 7
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

genFour' :: (Arbitrary a, Arbitrary b) => Gen (Four' a b)
genFour' = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four' w x y z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = genFour'


type Four'Compose = Fun String String -> Fun String String -> Four' String String -> Bool

-- 8

-- Not possible, because Functor requires a higher kinded type (* -> *)