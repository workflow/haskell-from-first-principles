isSubseqOf :: Eq a => [a] -> [a] -> Bool
isSubseqOf subSeq fullSeq = foldr (isSubseq) True $ comparisonPairs subSeq fullSeq

isSubseq :: Eq a => (a, [a]) -> Bool -> Bool
isSubseq (c, s) acc = (elem c s) && acc

comparisonPairs :: Eq a => [a] -> [a] -> [(a, [a])]
comparisonPairs subSeq fullSeq = zip subSeq (listOfRemainders subSeq fullSeq)

listOfRemainders :: Eq a => [a] -> [a] -> [[a]]
listOfRemainders subSeq fullSeq = tail $ scanl (\acc x -> dropWhile (/= x) acc) fullSeq subSeq

# Genious stuff from https://github.com/abevoelker/haskellbook-solutions/blob/master/ch11/exercises.hs
isSubseqOf' :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf' [] _  = True
isSubseqOf' _  [] = False
isSubseqOf' xs'@(x:xs) (y:ys)
  | x == y    = isSubseqOf xs  ys
  | otherwise = isSubseqOf xs' ys

main :: IO ()
main = do
    if
       (isSubseqOf' "blah" "blahwoot")
       && isSubseqOf' "blah" "wootblah"
       && isSubseqOf' "blah" "wboloath"
       && (isSubseqOf' "blah" "wootbla") == False
       && (isSubseqOf' "blah" "halbwoot") == False
       && isSubseqOf' "blah" "blawhoot"
    then putStrLn "Yipeeee!"
    else error "Deeerp."
