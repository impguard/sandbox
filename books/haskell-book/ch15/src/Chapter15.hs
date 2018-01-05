module Chapter15
  ( world
  , First'
  , Trivial
  , Two
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

first'Gen :: Arbitrary a => Gen (First' a)
first'Gen = do
  a <- arbitrary
  return $ First' (Only a)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = frequency [(1, return $ First' Nada), (3, first'Gen)]

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
  Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two m n) (Two o p) = Two (m <> o) (n <> p)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b
