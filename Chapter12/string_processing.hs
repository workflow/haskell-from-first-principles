import Data.List

-- Can use intercalate here or unwords next time :)
replaceThe :: String -> String
replaceThe s = concatMap replacer $ groupBy (\_ y -> y /= ' ') s where
    replacer :: String -> String
    replacer w
        | notThe w == Just w = w
        | otherwise = "a"

testReplaceThe :: IO ()
testReplaceThe = do
    putStrLn $ show $ replaceThe "the cow loves us"

notThe :: String -> Maybe String
notThe s
    | s == "the" = Nothing
    | otherwise = Just s

testNotThe :: IO ()
testNotThe = do
    putStrLn $ show $ notThe "the"
    putStrLn $ show $ notThe "blahtheblah"
    putStrLn $ show $ notThe "woot"

countTheBeforeVowel :: String -> Int
countTheBeforeVowel s = go (words s) where
    go :: [String] -> Int
    go [] = 0
    go (_:[]) = 0
    go (x:xs) = (if isTheBeforeVowel x $ head . head $ xs then 1 else 0) + go xs

isTheBeforeVowel :: String -> Char -> Bool
isTheBeforeVowel "" _ = False
isTheBeforeVowel s c
    | notThe s == Just s = False
    | otherwise = isVowel c

isVowel :: Char -> Bool
isVowel = (flip elem) "aeiou"

testCountTheBeforeVowel :: IO ()
testCountTheBeforeVowel = do
   putStrLn $ show $ countTheBeforeVowel "the cow"
   putStrLn $ show $ countTheBeforeVowel "the awesome cow"

countVowels :: String -> Int
countVowels s = length $ filter isVowel s

testCountVowels :: IO ()
testCountVowels = do
    putStrLn $ show $ countVowels "the cow"
    putStrLn $ show $ countVowels "Mikolajczak"
