-- Use zipWith next time :)
import Data.Char
import Control.Monad (forever)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

vigenere :: String -> String -> String
vigenere keyword plaintext = foldr vigenereFun "" (zip [0..] plaintext) where
    vigenereFun :: (Int, Char) -> String -> String
    vigenereFun (idx, c) acc = (vigShift (shiftCharAt keyword idx) c) : acc

shiftCharAt :: String -> Int -> Char
shiftCharAt keyword idx = (concat $ repeat keyword) !! idx

vigShift :: Char -> Char -> Char
vigShift shiftChar plainTextChar = rightShift (toBaseOrd shiftChar) plainTextChar

unvigenere :: String -> String -> String
unvigenere keyword ciphtext = foldr unvigenereFun "" (zip [0..] ciphtext) where
    unvigenereFun :: (Int, Char) -> String -> String
    unvigenereFun (idx, c) acc = (vigUnshift (shiftCharAt keyword idx) c) : acc

vigUnshift :: Char -> Char -> Char
vigUnshift shiftChar ciphChar = leftShift (toBaseOrd shiftChar) ciphChar

shift :: (Int -> Int -> Int) -> Int -> Char -> Char
shift f x c
    | isLower c = fromBaseOrd ((f (toBaseOrd c) x) `mod` 26) True
    | isUpper c = fromBaseOrd ((f (toBaseOrd c) x) `mod` 26) False
    | otherwise = c

rightShift :: Int -> Char -> Char
rightShift = shift (+)

leftShift :: Int -> Char -> Char
leftShift = shift (-)

toBaseOrd :: Char -> Int
toBaseOrd c = if isLower c then ord c `mod` 97 else ord c `mod` 65

fromBaseOrd :: Int -> Bool -> Char
fromBaseOrd x True = chr (97 + x)
fromBaseOrd x False = chr (65 + x)

caesar :: Int -> String -> String
caesar x plaintext = map (rightShift x) plaintext where

uncaesar :: Int -> String -> String
uncaesar x ciphtext = map (leftShift x) ciphtext where

interactiveVigenere :: IO ()
interactiveVigenere = do
    hSetBuffering stdout NoBuffering
    putStrLn "Encode your stuffs"
    putStrLn "Enter cipher word:"
    cipher <- getLine
    putStrLn "Enter plain text:"
    plaintext <- getLine
    putStrLn "Your encrypted stuff is:"
    putStrLn $ vigenere cipher plaintext
