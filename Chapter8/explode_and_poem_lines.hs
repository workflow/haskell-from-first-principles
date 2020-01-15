module ExplodeAndPoemLines where

explode :: Char -> String -> [String]
explode _ "" = []
explode sep s = [takeWhile (/= sep) s]  ++ explode sep remainder where
    remainder = dropWhile (== sep) . dropWhile(/= sep) $ s

myWords :: String -> [String]
myWords = explode ' '

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
    \ symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines = explode '\n'

shouldEqual =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?"
    ]

main :: IO()
main =
    print $
    "Are they equal?"
    ++ show (myLines sentences == shouldEqual)
