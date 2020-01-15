module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

digits :: Int -> [Int]
digits n = go n []
    where
        go 0 ns = ns
        go n ns = go allButLastDigit lastDigitAppended where
            allButLastDigit = div n 10
            lastDigit = mod n 10
            appendIntToList :: [Int] -> Int -> [Int]
            appendIntToList ns n = (++) ns $ (:[]) n
            lastDigitAppended = appendIntToList ns lastDigit

-- Returns "one-two-three-two-four-five-four-six" given 12324546 as input
wordNumber :: Int -> String
wordNumber n = concat . reverse . intersperse "-" . map digitToWord . digits $ n