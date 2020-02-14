fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

fibs20 = take 20 fibs

fibsSmaller100 = [x | x <- allFibs, x < 100 ]
    where allFibs = 1 : scanl (+) 1 fibs
fibsSmaller100' = scanl conditionalPlus 1 fibs where
    conditionalPlus x y
        | y < 100 = x + y
        | otherwise = x

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

factorial' :: Int -> Int
factorial' x = infFacs !! x where
    infFacs = scanl (*) 1 [1..x]
