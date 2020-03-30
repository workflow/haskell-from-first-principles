import Control.Monad
import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where 
    arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Semigroup Bull where 
    (<>) _ _ = Fools

instance Monoid Bull where
    mempty = Fools

type BullMAppend = Bull -> Bull -> Bull -> Bool

main :: IO ()
main = do
    let ma = monoidAssoc
        mli = monoidLeftIdentity
        mri = monoidRightIdentity
    quickCheck (ma :: BullMAppend)
    quickCheck (mli :: Bull -> Bool)
    quickCheck (mri :: Bull -> Bool)