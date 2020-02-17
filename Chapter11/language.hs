import Data.Char
import Data.List.Split (splitOn)

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (c:s) = if c == ' ' then " " ++ capitalizeWord s else toUpper c : s

capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph p = foldr (\x acc -> capitalizeWord x ++ "." ++ acc) "" (init (splitOn "." p))
