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
   quickCheck (functorIdentity :: Possibly Int -> Bool) 
   quickCheck (functorCompose' :: PossiblyCompose) 

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
    fmap f LolNope = LolNope
    fmap f (Yeppers a) = Yeppers $ f a

genYeppers :: Arbitrary a => Gen (Possibly a)
genYeppers = do
    x <- arbitrary
    return $ Yeppers x

instance Arbitrary a => Arbitrary (Possibly a) where
    arbitrary = frequency [ (1, genYeppers), (1, return LolNope) ]

type PossiblyCompose = Fun String String -> Fun String String -> Possibly String -> Bool