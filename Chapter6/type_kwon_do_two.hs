module TypeKwonDoTwo where

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b = aToB a == b

arith :: Num b
    => (a -> b)
    -> Integer
    -> a
    -> b
arith aToB _ a = aToB a
