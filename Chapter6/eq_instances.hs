module EQInstances where

data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
    (==) (TisAn x) (TisAn x') = x == x'


data TwoIntegers =
    Two Integer Integer
instance Eq TwoIntegers where
    (==) (Two x y) (Two x' y') =
        x == x' && y == y'

data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
    (==) (TisAnInt x) (TisAnInt x') = x == x'
    (==) (TisAString x) (TisAString x') = x == x'
    (==) (TisAnInt _) (TisAString _) = False
    (==) (TisAString _) (TisAnInt _) = False

data Pair a = Pair a a deriving Show
instance Eq a => Eq (Pair a) where
    (==) (Pair x y) (Pair x' y') = x == x' && y == y'

data Tuple a b = Tuple a b deriving Show
instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'

data Which a = ThisOne a | ThatOne a deriving Show
instance Eq a => Eq (Which a) where
    (==) (ThisOne x) (ThisOne x') = x == x'
    (==) (ThatOne x) (ThatOne x') = x == x'
    (==) (ThisOne _) (ThatOne _) = False
    (==) (ThatOne _) (ThisOne _) = False

data EitherOr a b = Hello a | Goodbye b deriving Show
instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello _) (Goodbye _) = False
    (==) (Goodbye _) (Hello _) = False
    (==) (Hello x) (Hello x') = x == x'
    (==) (Goodbye y) (Goodbye y') = y == y'


