asc :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
asc f a b c = f a (f b c) == f (f a b) c

ascFun :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
ascFun (<>) a b c = a <> (b <> c) == (a <> b) <> c

ascFun' :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
ascFun' (+) a b c = a + (b + c) == (a + b) + c

ascFun'' :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
ascFun'' (-) a b c = a - (b - c) == (a - b) - c