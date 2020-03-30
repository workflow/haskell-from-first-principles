module ListyInstances where

import Data.Monoid
import Listy

instance Monoid (Listy a) where
    mempty = Listy []