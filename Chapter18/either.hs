import Control.Monad (join)

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a) =  First a
    fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
    pure = Second
    (Second f) <*> (Second b) = Second $ f b
    (First a) <*> _ = First a
    _ <*> (First a) = First a

instance Monad (Sum a) where
    return = pure
    (First a) >>= _ = First a
    (Second b) >>= f = f b