import Data.Maybe

myIterate :: (a -> a) -> a -> [a]
myIterate f x = f x : myIterate f x

testMyIterate :: IO ()
testMyIterate = do
    putStrLn $ show $ take 20 $ iterate (+1) 0

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = fLeft ++ myUnfoldr f fRight where
    fResult = f x
    fLeft = maybe [] (\x -> [fst x]) fResult
    fRight = maybe x snd fResult

myUnfoldr' :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr' f b =
    case f b of 
        Nothing -> []
        Just (a, b') -> a : myUnfoldr' f b'


testMyUnfoldr :: IO ()
testMyUnfoldr = do
    putStrLn $ show $ take 20 $ myUnfoldr (\x -> Just (x, x+1)) 0

testMyUnfoldr' :: IO ()
testMyUnfoldr' = do
    putStrLn $ show $ take 20 $ myUnfoldr' (\x -> Just (x, x+1)) 0


betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\x -> Just (x, f x)) x 

testBetterIterate :: IO ()
testBetterIterate = do
    putStrLn $ show $ take 20 $ betterIterate (+1) 0