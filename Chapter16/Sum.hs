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
   quickCheck (functorIdentity :: Sum Int String -> Bool) 
   quickCheck (functorCompose' :: SumCompose) 

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (First a) = First a
    fmap f (Second b) = Second $ f b

genFirst :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
genFirst = do
    x <- arbitrary
    return $ First x

genSecond :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
genSecond = do
    x <- arbitrary
    return $ Second x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = frequency [ (1, genFirst), (1, genSecond) ]

type SumCompose = Fun String String -> Fun String String -> Sum String String -> Bool