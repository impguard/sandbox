module Main
  ( main
  ) where

import           Chapter15
import           Data.Monoid
import qualified Data.Semigroup  as S
import           Test.QuickCheck

prop_monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
prop_monoidAssoc a b c = a <> (b <> c) == (a <> b) <> c

prop_monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
prop_monoidLeftIdentity a = mempty <> a == a

prop_monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
prop_monoidRightIdentity a = a <> mempty == a

prop_semigroupAssoc :: (Eq m, S.Semigroup m) => m -> m -> m -> Bool
prop_semigroupAssoc a b c = a S.<> (b S.<> c) == (a S.<> b) S.<> c

type MonoidAssoc = First' String -> First' String -> First' String -> Bool

type MonoidId = First' String -> Bool

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

main :: IO ()
main
  -- monoid
 = do
  quickCheck (prop_monoidAssoc :: String -> String -> String -> Bool)
  quickCheck (prop_monoidLeftIdentity :: String -> Bool)
  quickCheck (prop_monoidRightIdentity :: String -> Bool)
  -- First'
  quickCheck (prop_monoidAssoc :: MonoidAssoc)
  quickCheck (prop_monoidLeftIdentity :: MonoidId)
  quickCheck (prop_monoidRightIdentity :: MonoidId)
  -- Semigroup Trivial
  quickCheck (prop_semigroupAssoc :: TrivialAssoc)
  -- Semigroup Two
  quickCheck (prop_semigroupAssoc :: TwoAssoc)
