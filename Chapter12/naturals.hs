data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = (+1) $ natToInteger $ n

integerToNat :: Integer -> Maybe Nat
integerToNat i
    | i >= 0 = Just $ validIntegerToNat i
    | otherwise = Nothing

validIntegerToNat :: Integer -> Nat
validIntegerToNat i
    | i == 0 = Zero
    | i > 0 = Succ $ validIntegerToNat (i-1)
    | otherwise = error "Runtime error baby!"

main :: IO ()
main = do
    putStrLn $ show $ natToInteger (Succ Zero)
    putStrLn $ show $ natToInteger (Succ (Succ Zero))
    putStrLn $ show $ integerToNat 0
    putStrLn $ show $ integerToNat 1
    putStrLn $ show $ integerToNat 2
    putStrLn $ show $ integerToNat (-1)