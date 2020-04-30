data Either a b = Left a | Right b deriving (Eq, Show)

instance Functor (Main.Either a) where
    fmap _ (Main.Left x) = Main.Left x
    fmap f (Main.Right y) = Main.Right $ f y

instance Applicative (Main.Either e) where
    pure = Main.Right
    Main.Left e <*> _ = Main.Left e
    Main.Right f <*> r = fmap f r

instance Foldable (Main.Either a) where
    foldMap _ (Main.Left _) = mempty
    foldMap f (Main.Right y) = f y
    
    foldr _ z (Main.Left _) = z
    foldr f z (Main.Right y) = f y z

instance Traversable (Main.Either a) where
    traverse _ (Main.Left x) = pure (Main.Left x)
    traverse f (Main.Right y) = Main.Right <$> f y




