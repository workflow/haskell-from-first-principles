{-# LANGUAGE FlexibleInstances #-}

newtype IntAndInt = IntAndInt (Int, Int) deriving Show

class TooMany a where
    tooMany :: a -> Bool

instance TooMany IntAndInt where
    tooMany (IntAndInt (x, y)) = x + y > 46

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (x, y) = True
