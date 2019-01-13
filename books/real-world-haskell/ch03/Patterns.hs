import Data.List (sortBy)

size :: [a] -> Int
size (_:xs) = 1 + (size xs)
size [] = 0

mean :: Fractional a => [a] -> a
mean xs = (sum xs) / (fromIntegral (size xs))

palindrome :: [a] -> [a]
palindrome ls = ls ++ reverse ls

is_palindrome :: Eq a => [a] -> Bool
is_palindrome ls = ls == reverse ls

sort_by_sublength :: [[a]] -> [[a]]
sort_by_sublength = sortBy (\i1 i2 -> compare (length i1) (length i2))

intersperse :: a -> [[a]] -> [a]
intersperse _ (x:[]) = x
intersperse s (x:y:[]) = x ++ [s] ++ y
intersperse s (x:xs) = x ++ [s] ++ (intersperse s xs)
intersperse _ [] = []

data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

height :: Tree a -> Int
height (Node _ n m) = 1 + max (height n) (height m)
height Empty = 0
