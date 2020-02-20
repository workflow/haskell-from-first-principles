newtype Word' = Word' String deriving (Eq, Show)


vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s
    | countVowels s <= countConsonants s = Just $ Word' s
    | otherwise = Nothing

isVowel :: Char -> Bool
isVowel = (flip elem) "aeiou"

isConsonant :: Char -> Bool
isConsonant c = not $ elem c "aeiou"

countVowels :: String -> Int
countVowels s = length $ filter isVowel s

countConsonants :: String -> Int
countConsonants s = length $ filter isConsonant s