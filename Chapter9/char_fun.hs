module CharFun where

import Data.Char

filterUppercase :: String -> String
filterUppercase s = filter isUpper s

ucFirst :: String -> String
ucFirst "" = ""
ucFirst (f:s) = toUpper f : s

allCaps :: String -> String
allCaps "" = ""
allCaps (c:cs) = toUpper c : allCaps cs

ucFirstOnly :: String -> Char
ucFirstOnly = toUpper . head
