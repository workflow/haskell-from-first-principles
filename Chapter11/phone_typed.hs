module Phone where

import Data.Maybe
import Data.List
import Data.Char
import Data.Ord


-- 1
data DaPhone = DaPhone [Button] deriving (Eq, Show)

data Button = Button {
    digit :: Digit,
    actions :: [Action]
    } deriving (Eq, Show)

daPhone = DaPhone [
    Button One "1",
    Button Two "abc2",
    Button Three "def3",
    Button Four "ghi4",
    Button Five "jkl5",
    Button Six "mno6",
    Button Seven "pqrs7",
    Button Eight "tuv8",
    Button Nine "wxyz9",
    Button Star "^*",
    Button Zero "+_0",
    Button Pound ".,#"
    ]

data Digit = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Star | Zero | Pound deriving (Eq, Show)
---- validActions = "A-Z + *^+_.,"
type Action = Char

--2
convo :: [String]
convo = ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol OK. Have you ever tasted alcohol",
    "Lol ya ya ya ya ya",
    "Wow ur cool ahaha. Ur turn",
    "OK. Do u think I am pretty lol",
    "Lol ya",
    "Just making sure rofl ur turn"]

-- Valid presses: 1 and up
type Presses = Int

buttonForAction :: DaPhone -> Action -> Button
buttonForAction (DaPhone ph) a = head [b | b@(Button _ as) <- ph, elem a as]

digitForAction :: DaPhone -> Action -> Digit
digitForAction ph = digit . buttonForAction ph

numPressesForAction :: DaPhone -> Action -> Presses
numPressesForAction ph a = (+1) $ fromJust $ elemIndex a (actions $ buttonForAction ph a)

reverseTaps :: DaPhone -> Action -> [(Digit, Presses)]
reverseTaps ph a
    | isUpper a = (Star, 1) : [(digitForAction ph $ toLower a, numPressesForAction ph $ toLower a)]
    | otherwise = [(digitForAction ph a, numPressesForAction ph a)]
--- assuming the default phone definition
--- 'a' -> [('2', 1)]
--- 'A' -> [('*', 1), ('2', 1)]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead ph = concatMap (reverseTaps ph)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr ((+) . snd) 0

mostPopularLetter :: String -> Char
mostPopularLetter xs = fst $ maximumBy (comparing snd) (frequencyOfElem xs)

frequencyOfElem :: Eq a => [a] -> [(a, Int)]
frequencyOfElem xs = [(e, length $ filter (== e) xs) | e <- nub xs]

coolestLtr :: [String] -> Char
coolestLtr =  mostPopularLetter . concat

mostPopularWord :: [String] -> String
mostPopularWord p = fst $ maximumBy (comparing snd) (frequencyOfWord $ p)

frequencyOfWord :: [String] -> [(String, Int)]
frequencyOfWord xs = [(e, length $ filter (== e) xs) | e <- nub xs]

coolestWord :: [String] -> String
coolestWord = mostPopularWord . words . concat
