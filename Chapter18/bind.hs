import Control.Monad (join)

-- join :: Monad m => m (m a) -> m a
-- fmap :: Functor m => (a -> b) -> m a -> m b

bind :: Monad m => (a -> m b) -> m a -> m b
bind f ma = join (fmap f ma) 

-- concat :: [[a]] -> [a]
-- map :: (a -> b) -> [a] -> [b]

lbind :: (a -> [b]) -> [a] -> [b]
lbind f as = concat (map f as)