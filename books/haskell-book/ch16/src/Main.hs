{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import           Control.Applicative (Applicative)
import           Test.QuickCheck     (CoArbitrary, Arbitrary, Fun, applyFun, arbitrary,
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
  arbitrary = frequency [(1, return LolNope), (3, Yeppers <$> arbitrary)]

type PossiblyId = Possibly String -> Bool

type PossiblyCompose = Fun String Int -> Fun Int Char -> Possibly String -> Bool

-- Chapter exercises (p. 679)
-- 2.
newtype K a b =
  K a
  deriving (Show, Eq)

instance Functor (K a) where
  fmap _ (K v) = K v

instance (Arbitrary a) => Arbitrary (K a b) where
  arbitrary = K <$> arbitrary

type KId = K String Int -> Bool

type KCompose = Fun String Int -> Fun Int Char -> K Int String -> Bool

-- 3.
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K x)) = Flip (K (f x))

instance (Arbitrary b) => Arbitrary (Flip K a b) where
  arbitrary = Flip <$> arbitrary

type FlipKId = Flip K String Int -> Bool

type FlipKCompose = Fun String Int -> Fun Int Char -> Flip K Int String -> Bool

-- 4.
newtype EvilGoateeConst a b =
  GoatyConst b
  deriving (Show, Eq)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst v) = GoatyConst (f v)

instance (Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
  arbitrary = GoatyConst <$> arbitrary

type EvilGoateeConstId = EvilGoateeConst String Int -> Bool

type EvilGoateeConstCompose
   = Fun Int String -> Fun String Char -> EvilGoateeConst Char Int -> Bool

-- 5.
newtype LiftItOut f a =
  LiftItOut (f a)
  deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

instance (Arbitrary a) => Arbitrary (LiftItOut Identity a) where
  arbitrary = LiftItOut <$> arbitrary

type LiftItOutId = LiftItOut Identity String -> Bool

type LiftItOutCompose
   = Fun String Int -> Fun Int Char -> LiftItOut Identity String -> Bool

-- 6.
data Parappa f g a =
  DaWrappa (f a)
           (g a)
  deriving (Show, Eq)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

instance (Arbitrary a) => Arbitrary (Parappa Identity Identity a) where
  arbitrary = DaWrappa <$> arbitrary <*> arbitrary

type ParappaId = Parappa Identity Identity String -> Bool

type ParappaCompose
   = Fun String Int -> Fun Int Char -> Parappa Identity Identity String -> Bool

-- 7.
data IgnoreOne f g a b =
  IgnoringSomething (f a)
                    (g b)
  deriving (Show, Eq)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

-- 9.
data List a
  = Nil
  | Cons a
         (List a)
  deriving (Show, Eq)

instance Functor (List) where
  fmap _ Nil         = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)

-- 11.
data TalkToMe a
  = Halt
  | Print String
          a
  | Read (String -> a)

instance Functor (TalkToMe) where
  fmap _ Halt        = Halt
  fmap f (Print s x) = Print s (f x)
  fmap f (Read func) = Read (fmap f func)

instance Show a => Show (TalkToMe a) where
  show Halt = "Halt"
  show (Print s x) = "Print " ++ s ++ " " ++ show x
  show (Read func) = "Read " ++ show (func "test")

-- Some helpers for testing
testFunctor ::
     (Show (f b), Eq (f b), Functor f) => (a -> b) -> f a -> f b -> IO ()
testFunctor f fa ga = do
  putStrLn $ "Expected " ++ (show ga)
  let result = (fmap f fa)
  putStrLn $ "Got      " ++ (show result)
  putStrLn $
    if ga == result
      then "         Success"
      else "         Failed"

main :: IO ()
main
  -- Exercises: Heavy Lifting (p. 656)
 = do
  putStrLn "Exercises: Heavy Lifting\n"
  -- 1.
  let a = (+ 1) <$> read "[1]" :: [Int]
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
  -- 4.
  quickCheck (functorId :: EvilGoateeConstId)
  quickCheck (functorCompose :: EvilGoateeConstCompose)
  -- 5.
  testFunctor (+ 1) (LiftItOut (Just 3)) (LiftItOut (Just 4))
  quickCheck (functorId :: LiftItOutId)
  quickCheck (functorCompose :: LiftItOutCompose)
  -- 6.
  let f6 = DaWrappa (Just 4) (Just 5)
  let g6 = DaWrappa (Just 8) (Just 10)
  testFunctor (* 2) f6 g6
  quickCheck (functorId :: ParappaId)
  quickCheck (functorCompose :: ParappaCompose)
  -- 7.
  let f7 = IgnoringSomething (Just 4) (Just "Someone")
  let g7 = IgnoringSomething (Just 4) (Just "WhyMe")
  testFunctor (const "WhyMe") f7 g7
  -- 9.
  let f9 = Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
  let g9 = Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil)))
  testFunctor (+ 1) f9 g9
