{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
functorCompose' (Fun _ f) (Fun _ g) x = (fmap g (fmap f x)) == (fmap (g . f) x)

main :: IO ()
main = do
   quickCheck (functorIdentity :: Quant Int Char -> Bool) 
   quickCheck (functorCompose' :: QuantCompose) 

   quickCheck (functorIdentity :: K Int Char -> Bool) 
   quickCheck (functorCompose' :: KCompose) 

   quickCheck (functorIdentity :: Flip K Int Char -> Bool) 
   quickCheck (functorCompose' :: FlipKCompose) 

   quickCheck (functorIdentity :: EvilGoateeConst Int Char -> Bool) 
   quickCheck (functorCompose' :: EvilGoateeConstCompose) 

   quickCheck (functorIdentity :: LiftItOut [] Char -> Bool) 
   quickCheck (functorCompose' :: LiftItOutCompose) 

   quickCheck (functorIdentity :: Parappa [] Maybe Char -> Bool) 
   quickCheck (functorCompose' :: ParappaCompose) 

   quickCheck (functorIdentity :: IgnoreOne [] Maybe Char Int -> Bool) 
   quickCheck (functorCompose' :: IgnoreOneCompose) 

   quickCheck (functorIdentity :: Notorious [] Bool Char Int -> Bool) 
   quickCheck (functorCompose' :: NotoriousCompose) 

   quickCheck (functorIdentity :: List Char -> Bool) 
   quickCheck (functorCompose' :: ListCompose) 

   quickCheck (functorIdentity :: GoatLord Int -> Bool) 
   quickCheck (functorCompose' :: GoatLordCompose) 

-- 1
data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a
    fmap f (Bloor b) = Bloor $ f b

genDesk :: (Arbitrary a, Arbitrary b) => Gen (Quant a b)
genDesk = do
    x <- arbitrary
    return $ Desk x

genBloor :: (Arbitrary a, Arbitrary b) => Gen (Quant a b)
genBloor = do
    x <- arbitrary
    return $ Bloor x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
    arbitrary = frequency [ (1, return Finance), (1, genDesk), (1, genBloor)]

type QuantCompose = Fun String String -> Fun String String -> Quant String String -> Bool

-- 2
data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
    fmap _ (K a) = K a

genK :: (Arbitrary a, Arbitrary b) => Gen (K a b)
genK = do
    x <- arbitrary
    return $ K x

instance (Arbitrary a, Arbitrary b) => Arbitrary (K a b) where
    arbitrary = genK

type KCompose = Fun String String -> Fun String String -> K String String -> Bool

-- 3
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
    fmap f (Flip (K a)) = Flip $ K $ f a

genFlipK :: (Functor (K a), Arbitrary (K a b), Arbitrary a, Arbitrary b) => Gen (Flip K a b)
genFlipK = do
    x <- arbitrary
    return $ Flip $ x

instance (Functor (K a), Arbitrary (K a b), Arbitrary a, Arbitrary b) => Arbitrary (Flip K a b) where
    arbitrary = genFlipK

type FlipKCompose = Fun String String -> Fun String String -> Flip K String String -> Bool

-- 4 
data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst $ f b

genGoatyConst :: (Arbitrary a, Arbitrary b) => Gen (EvilGoateeConst a b)
genGoatyConst = do
    x <- arbitrary
    return $ GoatyConst x

instance (Arbitrary a, Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
    arbitrary = genGoatyConst

type EvilGoateeConstCompose = Fun String String -> Fun String String -> EvilGoateeConst String String -> Bool

-- 5
data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut (fmap f fa) 

genLiftItOut :: (Functor f, Arbitrary (f a)) => Gen (LiftItOut f a)
genLiftItOut = do
    x <- arbitrary
    return $ LiftItOut x

instance (Functor f, Arbitrary (f a)) => Arbitrary (LiftItOut f a) where
    arbitrary = genLiftItOut

type LiftItOutCompose = Fun String String -> Fun String String -> LiftItOut [] String -> Bool

-- 6
data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

genParappa :: (Functor f, Functor g, Arbitrary (f a), Arbitrary (g a)) => Gen (Parappa f g a)
genParappa = do
    x <- arbitrary
    y <- arbitrary
    return $ DaWrappa x y

instance (Functor f, Functor g, Arbitrary (f a), Arbitrary (g a)) => Arbitrary (Parappa f g a) where
    arbitrary = genParappa

type ParappaCompose = Fun String String -> Fun String String -> Parappa [] [] String -> Bool

-- 7
data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

genIgnoreOne :: (Functor f, Functor g, Arbitrary (f a), Arbitrary (g b)) => Gen (IgnoreOne f g a b)
genIgnoreOne = do
    x <- arbitrary
    y <- arbitrary
    return $ IgnoringSomething x y

instance (Functor f, Functor g, Arbitrary (f a), Arbitrary (g b)) => Arbitrary (IgnoreOne f g a b) where
    arbitrary = genIgnoreOne

type IgnoreOneCompose = Fun String String -> Fun String String -> IgnoreOne [] [] String String -> Bool

-- 8
data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance (Functor g) => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt) 

genNotorious :: (Functor g, Arbitrary (g o), Arbitrary (g a), Arbitrary (g t)) => Gen (Notorious g o a t)
genNotorious = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Notorious x y z

instance (Functor g, Arbitrary (g o), Arbitrary (g a), Arbitrary (g t)) => Arbitrary (Notorious g o a t) where
    arbitrary = genNotorious

type NotoriousCompose = Fun String String -> Fun String String -> Notorious [] String String String -> Bool

-- 9
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a la) = Cons (f a) (fmap f la)

genCons :: (Arbitrary a, Arbitrary (List a)) => Gen (List a)
genCons = do
    x <- arbitrary
    y <- arbitrary
    return $ Cons x y

instance (Arbitrary a, Arbitrary (List a)) => Arbitrary (List a) where
    arbitrary = frequency [ (1, return Nil), (3, genCons) ]

type ListCompose = Fun String String -> Fun String String -> List String -> Bool

-- 10
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) deriving (Eq, Show)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat $ f a
    fmap f (MoreGoats gl1 gl2 gl3) = MoreGoats (fmap f gl1) (fmap f gl2) (fmap f gl3)

genOneGoat :: (Arbitrary a) => Gen (GoatLord a)
genOneGoat = do
    x <- arbitrary
    return $ OneGoat x

genMoreGoats :: (Arbitrary (GoatLord a)) => Gen (GoatLord a)
genMoreGoats = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ MoreGoats x y z

instance (Arbitrary a) => Arbitrary (GoatLord a) where
    arbitrary = frequency [ (3, return NoGoat), (1, genOneGoat), (1, genMoreGoats) ]

type GoatLordCompose = Fun String String -> Fun String String -> GoatLord String -> Bool

-- 11
data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print s a) = Print s $ f a
    fmap f (Read sa) = Read (fmap f sa)

-- Correcto or noto?
