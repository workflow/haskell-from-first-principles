data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Semigroup (Optional a) where
    (<>) Nada Nada = Nada
    (<>) Nada (Only x) = Only x
    (<>) (Only x) Nada = Only x
    (<>) (Only x) (Only y) = Only (x <> y)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada Nada = Nada
  mappend Nada (Only x) = Only x
  mappend (Only x) Nada = Only x
  mappend (Only x) (Only y) = Only (x `mappend` y)
