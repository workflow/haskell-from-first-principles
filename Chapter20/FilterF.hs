-- filterF :: ( Applicative f, Foldable t, Monoid (f a), Monoid a ) => (a -> Bool) -> t a -> f a
-- filterF g = pure . foldMap h where
--     h a 
--         | g a == True = a
--         | otherwise = mempty 

filterF :: ( Applicative f, Foldable t, Monoid (f a) ) => (a -> Bool) -> t a -> f a
filterF g = foldMap h where
    h a 
        | g a == True = pure a
        | otherwise = mempty 