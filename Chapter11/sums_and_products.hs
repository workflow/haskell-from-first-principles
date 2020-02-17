data GuessWhat = ChickenButt deriving (Eq, Show)

data Id a = MkId a deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b =
    First a
    | Second b
    deriving (Show, Eq)

data RecordProduct a b =
    RecordProduct { pfirst :: a
    , psecond :: b }
    deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)

--data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
--data Farmhouse' = Product NumCow NumPig

type Awesome = Bool
type Name = String

person :: Product Name Awesome
person = Product "Simon" True

data Twitter = Twitter deriving (Eq, Show)
data AskFm = AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter
