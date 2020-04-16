import Control.Monad

j :: Monad m => m (m a) -> m a
j = join
-- j = flip (>>=) id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap
-- l1 = flip (>>=) . (return.)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2
-- l2 f = ((.(>>=)).) $ flip (.) (>>=) (.(.((return.).) f))

a :: Monad m => m a -> m (a -> b) -> m b
a = flip ap
-- a ma mf = ma >>= (\a -> mf >>= (\f -> return $ f a))

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh (a:as) f = f a >>= (\b -> meh as f)

flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id