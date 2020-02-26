data BinaryTree a =
      Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case f a of
    Nothing -> Leaf
    Just (a', b, a'') -> Node (unfold f a') b (unfold f a'')

treeBuild :: Integer -> BinaryTree Integer
treeBuild n 
    | n < 0 = Leaf
    | otherwise = unfold f 0 where
            f k
                |  k == n = Nothing
                | otherwise = Just (k+1, k, k+1)

testTreeBuild :: IO ()
testTreeBuild = do 
    putStrLn $ show $ treeBuild 0
    putStrLn $ show $ treeBuild 1
    putStrLn $ show $ treeBuild 2
    putStrLn $ show $ treeBuild 3