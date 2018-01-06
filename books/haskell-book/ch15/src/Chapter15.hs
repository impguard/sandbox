module Chapter15
  ( world
  , First'
  , Trivial
  , Two
  , Three
  , BoolConj
  , Combine(unCombine)
  , BoolDisj
  , Mem(runMem)
  ) where

import           Data.Semigroup
import           Test.QuickCheck

world = " world!"

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only x) (Only y) = Only $ mappend x y
  mappend Nada y            = y
  mappend x Nada            = x

newtype First' a = First'
  { getFirst' :: Optional a
  } deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) b = b
  mappend a (First' Nada) = a
  mappend a _             = a

firstGen :: Arbitrary a => Gen (First' a)
firstGen = do
  a <- arbitrary
  return $ First' (Only a)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = frequency [(1, return $ First' Nada), (3, firstGen)]

-- Semigroup Exercises
-- 1.
data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

-- 3.
data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two m n) (Two o p) = Two (m <> o) (n <> p)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

-- 4.
data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>
         Semigroup (Three a b c) where
  (<>) (Three a b c) (Three d e f) = Three (a <> d) (b <> e) (c <> f)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

-- 6.
newtype BoolConj =
  BoolConj Bool
  deriving (Show, Eq)

instance Semigroup BoolConj where
  (<>) (BoolConj True) (BoolConj True) = BoolConj True
  (<>) _ _                             = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

-- 9.
newtype Combine a b = Combine
  { unCombine :: a -> b
  }

instance Semigroup b => Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) = Combine $ \x -> f x <> g x

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

-- Monoid Exercises
-- 5.
newtype BoolDisj =
  BoolDisj Bool
  deriving (Show, Eq)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend (BoolDisj True) (BoolDisj True)   = BoolDisj False
  mappend (BoolDisj False) (BoolDisj False) = BoolDisj False
  mappend _ _                               = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

-- 8.
newtype Mem s a = Mem
  { runMem :: s -> (a, s)
  }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend (Mem f) (Mem g) =
    Mem $ \s ->
      let (a, s') = g s
          (b, s'') = f s'
      in (mappend a b, s'')
