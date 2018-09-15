module Simple where

import Data.List (elemIndex)
import Data.Monoid
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- p.1076-1077

lookups :: IO()
lookups = do
  print $ fmap (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])
  let d = zip [1, 2, 3] [4, 5, 6]
      y = lookup 3 d
      z = lookup 2 d
   in print $ (,) <$> y <*> z
  let l = [1, 2, 3, 4, 5]
      x = elemIndex 3 l
      y = elemIndex 4 l
      max' :: Int -> Int -> Int
      max' = max
   in print $ max' <$> x <*> y
  let xs = [1, 2, 3]
      ys = [4, 5, 6]
      x = lookup 3 $ zip xs ys
      y = lookup 2 $ zip xs ys
   in print $ sum <$> ((,) <$> x <*> y)

-- p.1079 Identity Applicative

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity f) a = fmap f a

-- p.1115 Playing with checkers

data Bull =
    Folse
  | Troo
  deriving (Eq, Show)

instance Semigroup Bull where
  _ <> _ = Folse

instance Monoid Bull where
  mempty = Folse
  mappend = (<>)

instance Arbitrary Bull where
  arbitrary = oneof [return Folse, return Troo]

instance EqProp Bull where
  (=-=) = eq

bull :: IO()
bull = quickBatch (monoid Troo)

testLs = quickBatch $ applicative (undefined :: [(String, String, Int)])

-- p.1123 Playing with checkers with Ziplists

instance Semigroup a
      => Semigroup (ZipList a) where
  (<>) = liftA2 (<>)

instance Monoid a
      => Monoid (ZipList a) where
  mempty = ZipList [] -- This should be pure mempty to work...
  mappend = (<>)

instance Eq a => EqProp (ZipList a) where
  (=-=) = eq

-- p.1125 List Applicative

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)


(+++) :: List a -> List a -> List a
(+++) Nil l = l
(+++) (Cons x xs) ys = Cons x (xs +++ ys)

instance Applicative List where
  pure a = Cons a Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  Cons f fs <*> list = (fmap f list) +++ (fs <*> list)

instance Arbitrary a
      => Arbitrary (List a) where
  arbitrary =
    frequency [
      (1, return Nil),
      (3, arbitrary >>= \a -> return $ Cons a Nil)
    ]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

testList = quickBatch $ applicative (undefined :: List (String, String, Int))

list :: IO ()
list = do
  let f = Cons (+1) (Cons (*2) Nil)
      v = Cons 1 (Cons 2 Nil)
   in print $ f <*> v -- Expect Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

-- p. 1128 ZipList Applicative

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _ = Nil
take' a (Cons x xs) = Cons x $ take' (a - 1) xs

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                 in take' 3000 l
          ys' = let (ZipList' l) = ys
                 in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' (pure a)
  ZipList' Nil <*> _ = ZipList' Nil
  _ <*> ZipList' Nil = ZipList' Nil
  ZipList' fs <*> ZipList' xs = ZipList' (zipmap fs xs)
    where zipmap Nil _ = Nil
          zipmap _ Nil = Nil
          zipmap (Cons f fs) (Cons x xs) = Cons (f x) (zipmap fs xs)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

testZipList = quickBatch $ applicative (undefined :: ZipList' (String, String, Int))

repeat' :: Int -> List Int
repeat' n = Cons n (repeat' n)

ziplist :: IO ()
ziplist = do
  let zl' = ZipList'
      z = zl' $ Cons (+9) (Cons (*2) (Cons (+8) Nil))
      z' = zl' $ Cons 1 (Cons 2 (Cons 3 Nil))
   in do print $ z <*> z' -- Expected 10, 4, 11
         print $ z <*> zl' (repeat' 1) -- Expected 10, 2, 9

-- p.1134 Validation Applicative

data Validation e a =
    Error e
  | Succeed a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Error e) = Error e
  fmap f (Succeed a) = Succeed (f a)

instance Monoid e
      => Applicative (Validation e) where
  pure a = Succeed a
  Error e1 <*> Error e2 = Error (e1 <> e2)
  Error e <*> _ = Error e
  _ <*> Error e = Error e
  Succeed f <*> Succeed a = Succeed (f a)

instance (Arbitrary e, Arbitrary a)
      => Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    oneof [return (Error e), return (Succeed a)]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

testValidation = quickBatch $ applicative (undefined :: Validation [String] (String, String, Int))

-- p. 1137 Combinations

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
