module FoldBool where

foldBool :: a -> a -> Bool -> a
foldBool x y switch = case switch of
    True -> y
    False -> x

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y switch
    | switch == True = y
    | switch == False = x

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True = y

g :: (a -> b) -> (a, c) -> (b, c)
g aToB (a, c) = (aToB a, c)
