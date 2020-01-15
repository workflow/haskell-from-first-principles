module Cipher where

import Data.Char

caesar :: Int -> String -> String
caesar _ "" = ""
caesar x (c:cs) = shiftit x c : caesar x cs where
    shiftit x c = fromBaseOrd . mod ((+x) . toBaseOrd $ c) $ 26

uncaesar :: Int -> String -> String
uncaesar _ "" = ""
uncaesar x (c:cs) = unshiftit x c : uncaesar x cs where
    unshiftit x c = fromBaseOrd . mod ( toBaseOrd c - x ) $ 26

toBaseOrd :: Char -> Int
toBaseOrd c
    | isLower c = ord c - ord 'a'
    | otherwise = undefined

fromBaseOrd :: Int -> Char
fromBaseOrd x
    | x < 26 = chr (x + ord 'a')
    | otherwise = undefined
