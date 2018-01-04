module Main
  ( module Main
  ) where

import           Data.List       (sort)
import qualified Data.Map        as M
import           Morse
import           Test.QuickCheck

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
  forAll charGen (\c -> ((charToMorse c) >>= morseToChar) == Just c)

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
prop_plusAssociative x y z =
  x + (y + z) == (x + y) + z

prop_plusCommutative :: Int -> Int -> Bool
prop_plusCommutative x y = x + y == y + x

main :: IO ()
main = do
  quickCheck (prop_halfIdentity :: Double -> Bool)
  quickCheck (prop_isListOrdered :: [Int] -> Bool)
  quickCheck (prop_isListOrdered :: [String] -> Bool)
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative
