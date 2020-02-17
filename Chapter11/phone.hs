module Phone where

import Data.Maybe
import Data.List
import Data.Char


-- 1
type DaPhone = [Button]

type Button = (Digit, [Action])

daPhone = [
    ('1', "1"),
    ('2', "abc2"),
    ('3', "def3"),
    ('4', "ghi4"),
    ('5', "jkl5"),
    ('6', "mno6"),
    ('7', "pqrs7"),
    ('8', "tuv8"),
    ('9', "wxyz9"),
    ('*', "^*"),
    ('0', "+_0"),
    ('#', ".,#")
    ]

-- validButtons = "1234567890*#"
type Digit = Char
-- validActions = "A-Z + *^+_.,"
type Action = Char

--2
convo :: [String]
convo = ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol OK. Have you ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "OK. Do u think I am pretty lol",
    "Lol ya",
    "Just making sure rofl ur turn"]

-- Valid presses: 1 and up
type Presses = Int

buttonForAction :: DaPhone -> Action -> Button
buttonForAction ph a = head $ filter (\t -> elem a (snd t)) ph

digitForAction :: DaPhone -> Action -> Digit
digitForAction ph a = fst (buttonForAction ph a)

numPressesForAction :: DaPhone -> Action -> Presses
numPressesForAction ph a = (+1) $ fromJust $ elemIndex a (snd $ buttonForAction ph a)

reverseTaps :: DaPhone -> Action -> [(Digit, Presses)]
reverseTaps ph a
    | isUpper a = ('*', 1) : [(digitForAction ph $ toLower a, numPressesForAction ph $ toLower a)]
    | otherwise = [(digitForAction ph a, numPressesForAction ph a)]
--- assuming the default phone definition
--- 'a' -> [('2', 1)]
--- 'A' -> [('*', 1), ('2', 1)]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead ph s = concat $ map (reverseTaps ph) s

