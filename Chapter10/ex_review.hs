stops = "pbtdkg"
vowels = "aeiou"

allTuples :: String -> String -> [(Char, Char, Char)]
allTuples stops vowels = [(x, y, z) | x <- stops, y <- vowels, z <- stops]

allPTuples :: String -> String -> [(Char, Char, Char)]
allPTuples stops vowels = [(x, y, z) | x <- "p", y <- vowels, z <- stops]

nouns = ["beauty", "breed", "sex", "anomaly", "wormhole"]
verbs = ["fuck", "swear", "roar", "ride"]

allNounVerbTuples :: [String] -> [String] -> [(String, String, String)]
allNounVerbTuples nouns verbs = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns]

fracAvgWordLength :: Fractional a => String -> a
fracAvgWordLength x = (/) (fromIntegral (sum (map length (words x)))) (fromIntegral (length (words x)))
