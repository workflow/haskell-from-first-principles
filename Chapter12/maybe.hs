isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

testIsJust :: IO ()
testIsJust = do
    putStrLn $ show $ isJust (Just 1)
    putStrLn $ show $ isJust Nothing

isNothing :: Maybe a -> Bool
isNothing = not . isJust

testIsNothing :: IO ()
testIsNothing = do
    putStrLn $ show $ isNothing (Just 1)
    putStrLn $ show $ isNothing Nothing

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f Nothing = b
mayybee b f (Just a) = f a

testMayybee :: IO ()
testMayybee = do
    putStrLn $ show $ mayybee 0 (+1) Nothing
    putStrLn $ show $ mayybee 0 (+1) (Just 1)

fromMaybe :: a -> Maybe a -> a
fromMaybe = (flip mayybee) id

testFromMaybe :: IO ()
testFromMaybe = do
    putStrLn $ show $ fromMaybe 0 Nothing
    putStrLn $ show $ fromMaybe 0 (Just 1)

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

-- Weird type err here
-- testListToMaybe :: IO ()
-- testListToMaybe = do
--     putStrLn $ show $ listToMaybe [1, 2, 3]
--     putStrLn $ show $ listToMaybe []

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- Weird type err here
-- testListToMaybe :: IO ()
-- testListToMaybe = do
--     putStrLn $ show $ maybeToList (Just 1)
--     putStrLn $ show $ maybeToList Nothing

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:ms) = catMaybes ms
catMaybes (Just m:ms) = m : catMaybes ms
    

testCatMaybes :: IO ()
testCatMaybes = do
    putStrLn $ show $ catMaybes [Just 1, Nothing, Just 2]
    -- Weird type err here
    -- putStrLn $ show $ catMaybes $ take 3 $ repeat Nothing

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs
    | all isJust xs = Just $ catMaybes xs
    | otherwise = Nothing


testFlipMaybe :: IO ()
testFlipMaybe = do
    putStrLn $ show $ flipMaybe [Just 1, Just 2, Just 3]
    -- Weird type err here
    putStrLn $ show $ flipMaybe [Just 1, Nothing, Just 3]