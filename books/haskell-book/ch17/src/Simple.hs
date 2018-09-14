module Simple where

import Data.List (elemIndex)

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
