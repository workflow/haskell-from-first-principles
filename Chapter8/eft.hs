module EFT where

eftBool :: Bool -> Bool -> [Bool]
eftBool = myEnumFromTo

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = myEnumFromTo

eftInt :: Int -> Int -> [Int]
eftInt = myEnumFromTo

eftChar :: Char -> Char -> [Char]
eftChar = myEnumFromTo

myEnumFromTo :: Enum a => a -> a -> [a]
myEnumFromTo from to
    | (fromEnum from) == (fromEnum to) = [from]
    | (fromEnum from) > (fromEnum to) = []
    | otherwise = [from] ++ myEnumFromTo (toEnum . (+1) . fromEnum $ from) to
