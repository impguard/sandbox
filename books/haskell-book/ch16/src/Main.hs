{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import           Control.Applicative (Applicative)
import           Test.QuickCheck     (Arbitrary, Fun, applyFun, arbitrary,
                                      frequency, quickCheck)

functorId :: (Functor f, Eq (f a)) => f a -> Bool
functorId f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => Fun a b -> Fun b c -> f a -> Bool
functorCompose f g x = fmap g' (fmap f' x) == fmap (g' . f') x
  where
    f' = applyFun f
    g' = applyFun g

-- Exercises: Instances of Func (p. 663)
-- 1.
newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

type IdentityId = Identity String -> Bool

type IdentityCompose = Fun String Int -> Fun Int Char -> Identity String -> Bool

-- 7.
data Four' a b =
  Four' a
        a
        a
        b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

type FourId' = Four' String Int -> Bool

type FourCompose' = Fun String Int -> Fun Int Char -> Four' Int String -> Bool

-- Exercise: Possibly (p. 666)
data Possibly a
  = LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope     = LolNope
  fmap f (Yeppers x) = Yeppers (f x)

instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary =
    frequency [(1, return LolNope), (3, Yeppers <$> arbitrary)]

type PossiblyId = Possibly String -> Bool

type PossiblyCompose = Fun String Int -> Fun Int Char -> Possibly String -> Bool

-- Chapter exercises (p. 679)
-- 2.
newtype K a b = K a deriving (Show, Eq)

instance Functor (K a) where
  fmap _ (K v) = (K v)

instance (Arbitrary a) => Arbitrary (K a b) where
  arbitrary = K <$> arbitrary

type KId = K String Int -> Bool

type KCompose = Fun String Int -> Fun Int Char -> K Int String -> Bool

-- 3.
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K x)) = Flip (K (f x))

instance (Arbitrary b) => Arbitrary (Flip K a b) where
  arbitrary = Flip <$> arbitrary

type FlipKId = Flip K String Int -> Bool

type FlipKCompose = Fun String Int -> Fun Int Char -> Flip K Int String -> Bool

-- newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

main :: IO ()
main
  -- Exercises: Heavy Lifting (p. 656)
 = do
  putStrLn "Exercises: Heavy Lifting\n"
  -- 1.
  let a = fmap (+ 1) $ read "[1]" :: [Int]
  print a
  -- 2.
  let b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
  print b
  -- 3.
  let c = fmap (* 2) (\x -> x - 2)
  print $ c 1
  -- 4.
  let d = fmap ((return '1' ++) . show) (\x -> [x,1 .. 3])
  print $ d 0
  -- 5.
  let e =
        let ioi = readIO "1" :: IO Integer
            changed = fmap read $ fmap ("123" ++) (fmap show ioi)
        in fmap (* 3) changed
  e' <- e
  print e'
  -- Exercises: Instances of Func (p. 663)
  putStrLn "\nExercises: Instances of Func\n"
  -- 1.
  quickCheck (functorId :: IdentityId)
  quickCheck (functorCompose :: IdentityCompose)
  -- 7.
  quickCheck (functorId :: FourId')
  quickCheck (functorCompose :: FourCompose')
  -- Exercise: Possibly (p. 666)
  quickCheck (functorId :: PossiblyId)
  quickCheck (functorCompose :: PossiblyCompose)
  -- Chapter Exercises (p. 679)
  -- 2.
  quickCheck (functorId :: KId)
  quickCheck (functorCompose :: KCompose)
  -- 3.
  quickCheck (functorId :: FlipKId)
  quickCheck (functorCompose :: FlipKCompose)
