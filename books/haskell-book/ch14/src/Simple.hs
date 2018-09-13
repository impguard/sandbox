module Simple where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

-- Simple Arbitrary

data Trivial =
  Trivial
  deriving (Eq, Show)

instance Arbitrary Trivial where
  arbitrary = return Trivial

-- Identity

data Identity a =
  Identity a
  deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

-- Product Types

data Pair a b = Pair a b deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = pairGen

-- Sum Types

data Sum a b = First a | Second b deriving (Eq, Show)

sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [return (First a), return (Second b)]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

main :: IO()
main = do
  sample (arbitrary :: Gen Trivial)
  sample identityGenInt
  sample (arbitrary :: Gen (Pair String Int))
  sample sumGenCharInt
