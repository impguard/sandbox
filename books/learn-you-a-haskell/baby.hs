doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x * 2

removeNonUppercase :: String -> String
removeNonUppercase str = [ c | c <- str, c `elem` ['A'..'Z'] ]


quicksort :: Ord a => [a] -> [a]
quicksort []  = []
quicksort (pivot:rest) =
    let less = quicksort (filter (<= pivot) rest)
        more = quicksort (filter (> pivot) rest)
    in less ++ [pivot] ++ more

firsthalf :: [a] -> [a]
firsthalf [] = []
firsthalf xs = take (length xs `div` 2) xs

secondhalf :: [a] -> [a]
secondhalf [] = []
secondhalf xs = drop (length xs `div` 2) xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x < y = x:(merge xs (y:ys))
    | otherwise = y:(merge (x:xs) ys)

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort (firsthalf xs)) (mergesort (secondhalf xs))

divideBy10 = (/10)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

sumOddSquares = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

chain :: Integral a => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | otherwise = n:chain (n * 3 + 1)

numLongChains = length $ filter ((>15) . length . chain) [1..100]

shadowName = filter
    where filter = 3

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = abs $ (y2 - y1) * (x2 - x1)
