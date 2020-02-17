import Data.Char

capitalizeWords :: String -> [(String, String)]
capitalizeWords "" = []
capitalizeWords s = wordsToTuples (words s) where
    wordsToTuples :: [String] -> [(String, String)]
    wordsToTuples [] = []
    wordsToTuples (x:xs) =  (x, ucfirst x) : (wordsToTuples xs)

    ucfirst :: String -> String
    ucfirst "" = ""
    ucfirst (c:s) = (toUpper c) : s

capitalizeWords' :: String -> [(String, String)]
capitalizeWords' "" = []
capitalizeWords' s  = wordsToTuples (words s) where
    wordsToTuples :: [String] -> [(String, String)]
    wordsToTuples [] = []
    wordsToTuples (x@(c:s):xs) =  (x, (toUpper c) : s) : (wordsToTuples xs)
