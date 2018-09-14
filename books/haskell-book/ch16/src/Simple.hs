module Simple where

import Test.QuickCheck
import Test.QuickCheck.Function

-- Functor Laws

functorIdentity :: (Functor f, Eq (f a))
                => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c))
               => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g . fmap f) x == fmap (g . f) x

functorCompose' :: (Functor f, Eq (f c))
                => Fun a b -> Fun b c -> f a -> Bool
functorCompose' (Fun _ f) (Fun _ g) x =
  (fmap g . fmap f) x == fmap (g . f) x

-- p.983 WhoCares

data WhoCares a =
    ItDoesnt
  | Matter a
  | WhatThisIsCalled deriving (Show)

instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a) = Matter (f a)

-- p.1004

lifting :: IO()
lifting = do
  print (fmap (+1) $ read "[1]" :: [Int]) -- Expect [2]
  print (
    fmap (("1" ++) . show)
    (\x -> [x, 1..3]) 0
    ) -- Expect 1[0,1,2,3]
  result <- let ioi = readIO "1" :: IO Integer
                changed = fmap read (fmap ("123"++) (fmap show ioi))
             in fmap (*3) changed
  print result -- Expected 3693

-- p.1015 Four Functor

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
         => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

type FourId = Four Int Int Int Int -> Bool
type FourCompose =
     Fun Int String
  -> Fun String Char
  -> Four Int Int Int Int
  -> Bool

four :: IO()
four = do
  quickCheck (functorIdentity :: FourId)
  quickCheck (functorCompose' :: FourCompose)

-- p.1015 Four' Functor

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b)
    => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four' a b c d

type FourId' = Four' Int String -> Bool
type FourCompose' =
     Fun String Int
  -> Fun Int Char
  -> Four' Int String
  -> Bool

four' :: IO()
four' = do
  quickCheck (functorIdentity :: FourId')
  quickCheck (functorCompose' :: FourCompose')

-- p.1015

{- You can't implement a functor for
 -   data Trivial = Trivial
 - because it's not a higher kinded type. In essence the type constructor
 - Trivial is nullary so it's a type constant. As a result, the data
 - constructors are also nullary so they're just data constants.
 -
 - The kind for Trivial is simply '*', which can be expressed as not being a
 - higher kinded type. All functor instances must be higher kinded.
 -}
