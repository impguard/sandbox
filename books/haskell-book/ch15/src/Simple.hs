module Simple where

import Test.QuickCheck

-- Monoid laws

monoidAssoc :: (Eq m, Monoid m)
            => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m)
                   => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m)
                    => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- Semigroup laws

semigroupAssoc :: (Eq s, Semigroup s)
               => s -> s -> s -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

-- Booly

data Booly a = False' | True' deriving (Eq, Show)

instance Semigroup (Booly a) where
  False' <> _ = False'
  _ <> False' = False'
  True' <> True' = True'

instance Monoid (Booly a) where
  mempty = True'
  mappend = (<>)

-- Optional

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

-- Custom monoidal form for optional

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Semigroup (First' a) where
  First' Nada <> a = a
  a <> First' Nada = a
  First' a <> _ = First' a

instance Monoid (First' a) where
  mempty = First' Nada
  mappend = (<>)

firstGen :: Arbitrary a => Gen (First' a)
firstGen = do
  a <- arbitrary
  return $ First' (Only a)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = frequency [(1, return $ First' Nada), (3, firstGen)]

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId = First' String -> Bool

-- p.945 Two Semigroup

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b)
    => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)


twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return $ Two a b

instance (Arbitrary a, Arbitrary b)
    => Arbitrary (Two a b) where
  arbitrary = twoGen

type TwoAssoc =
     Two String [Int]
  -> Two String [Int]
  -> Two String [Int]
  -> Bool

-- p.949 Validation Semigroup

data Validation a b =
  Error a | Yes b
  deriving (Eq, Show)

instance Semigroup a =>
    Semigroup (Validation a b) where
  Yes a <> _ = Yes a
  _ <> Yes a = Yes a
  Error a <> Error b = Error (a <> b)

validationGen :: (Arbitrary a, Arbitrary b)
  => Gen (Validation a b)
validationGen = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ Error a, return $ Yes b]

instance (Arbitrary a, Arbitrary b)
    => Arbitrary (Validation a b) where
  arbitrary = validationGen

type ValidationAssoc =
     Validation String Int
  -> Validation String Int
  -> Validation String Int
  -> Bool

-- p.953 Mem Monoid

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Semigroup a => Semigroup (Mem s a) where
  m1 <> m2 = Mem $ \s1 ->
    let (a, s2) = runMem m1 s1
        (b, s3) = runMem m2 s2
     in (a <> b, s3)

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend = (<>)

f' = Mem $ \s -> ("hi", s + 1)

-- Tests

main :: IO()
main = do
  print "First' Tests"
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
  print "Two Tests"
  quickCheck (semigroupAssoc :: TwoAssoc)
  print "Validation Tests"
  quickCheck (semigroupAssoc :: ValidationAssoc)
  print "Mem Tests"
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0
