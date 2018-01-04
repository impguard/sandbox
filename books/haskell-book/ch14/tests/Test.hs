module Main
  ( module Main
  ) where

import           Data.List                 (sort)
import qualified Data.Map                  as M
import           Morse
import           Test.QuickCheck
import           Test.QuickCheck.Modifiers

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
  forAll charGen (\c -> (charToMorse c >>= morseToChar) == Just c)

-- Using QuickCheck
-- 1.
half :: Fractional a => a -> a
half x = x / 2

prop_halfIdentity :: (Eq a, Fractional a) => a -> Bool
prop_halfIdentity x = half x * 2 == x

-- 2.
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t)      = (Just y, t)
    go y (Just x, t)       = (Just y, x >= y)

prop_isListOrdered :: (Ord a) => [a] -> Bool
prop_isListOrdered = listOrdered . sort

-- 3.
prop_plusAssociative :: Int -> Int -> Int -> Bool
prop_plusAssociative x y z = x + (y + z) == (x + y) + z

prop_plusCommutative :: Int -> Int -> Bool
prop_plusCommutative x y = x + y == y + x

-- 5.
prop_quotremIdentity :: NonZero Int -> NonZero Int -> Bool
prop_quotremIdentity (NonZero x) (NonZero y) = quot x y * y + rem x y == x

prop_divmodIdentity :: NonZero Int -> NonZero Int -> Bool
prop_divmodIdentity (NonZero x) (NonZero y) = div x y * y + mod x y == x

-- 6.
-- Both are false
prop_powerAssociative :: Int -> Int -> Int -> Bool
prop_powerAssociative x y z = x ^ y ^ z == (x ^ y) ^ z

prop_powerCommutative :: Int -> Int -> Bool
prop_powerCommutative x y = x ^ y == y ^ x

--7.
prop_reverseIdentity :: (Eq a) => [a] -> Bool
prop_reverseIdentity x = (reverse . reverse) x == id x

-- Failure
square :: (Floating a) => a -> a
square x = x * x

prop_squareIdentity :: (Eq a, Floating a) => a -> Bool
prop_squareIdentity x = (square . sqrt) x == x

-- Fails because of floating point precision most likely
-- Make a Gen random generator for the datatype
-- 1.
data Fool
  = Fulse
  | Frue
  deriving (Eq, Show)

genFool :: Gen Fool
genFool = elements [Fulse, Frue]


-- 2.
genWeightedFool :: Gen Fool
genWeightedFool = frequency [(1, return Frue), (2, return Fulse)]


main :: IO ()
main = do
  quickCheck (prop_halfIdentity :: Double -> Bool)
  quickCheck (prop_isListOrdered :: [Int] -> Bool)
  quickCheck (prop_isListOrdered :: [String] -> Bool)
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative
  quickCheck prop_quotremIdentity
  quickCheck prop_divmodIdentity
  quickCheck (prop_reverseIdentity :: [Int] -> Bool)
  putStrLn "Fool"
  sample genFool
  putStrLn "Weighted Fool"
  sample genWeightedFool
