import           Data.Char (toLower)

isVowel :: Char -> Bool
isVowel = flip elem "aeiou" . toLower

-- String Processing
-- 1.
notThe :: String -> Maybe String
notThe s =
  if s == "the"
    then Nothing
    else Just s

replaceThe :: String -> String
replaceThe s =
  let maybeA = fromMaybe "a"
  in unwords $ map (maybeA . notThe) $ words s

-- 2.
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go 0 . words
  where
    go count (x:xs)
      | x == "the" = go (count + 1) xs
      | isVowel (head x) = count
      | otherwise = go count xs
    go _ [] = 0

-- 3.
countVowels :: String -> Int
countVowels = length . filter isVowel

-- Validate the word
newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s =
  if go 0 0 s
    then Nothing
    else Just (Word' s)
  where
    go :: Integer -> Integer -> String -> Bool
    go vowelCount consonantCount (x:xs)
      | isVowel x = go (vowelCount + 1) consonantCount xs
      | otherwise = go vowelCount (consonantCount + 1) xs
    go vowelCount consonantCount [] = vowelCount > consonantCount

-- It's only Natural
data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0 = Nothing
  | otherwise = Just (go n)
  where
    go 0    = Zero
    go posn = Succ $ go (posn - 1)

-- Small library for Maybe
isJust :: Maybe a -> Bool
isJust m =
  case m of
    Just _  -> True
    Nothing -> False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee d t m =
  case m of
    Nothing -> d
    Just v  -> t v

fromMaybe :: a -> Maybe a -> a
fromMaybe fallback m =
  case m of
    Nothing -> fallback
    Just v  -> v

listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just v) = [v]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) =
  case x of
    Nothing -> catMaybes xs
    Just v  -> v : catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = go []
  where
    go curr (m:ms) =
      case m of
        Nothing -> Nothing
        Just v  -> go (v : curr) ms
    go curr [] = Just (reverse curr)

-- Small libary for Either
lefts' :: [Either a b] -> [a]
lefts' = foldr op []
  where
    op e xs =
      case e of
        Left v -> v : xs
        _      -> xs

rights' :: [Either a b] -> [b]
rights' = foldr op []
  where
    op e xs =
      case e of
        Right v -> v : xs
        _       -> xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' es = (lefts' es, rights' es)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right v) = Just (f v)
eitherMaybe' _ _         = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left v)  = f v
either' _ g (Right v) = g v

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

-- Unfolds
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x =
  case f x of
    Nothing     -> []
    Just (a, b) -> a : myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f =
  let op x = Just (x, f x)
  in myUnfoldr op

-- Finally something other than a list!
data BinaryTree a
  = Leaf
  | Node (BinaryTree a)
         a
         (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x =
  case f x of
    Nothing        -> Leaf
    Just (m, y, n) -> Node (unfold f m) y (unfold f n)

treeBuild :: Integer -> BinaryTree Integer
treeBuild x =
  let op y = if y == x then Nothing else Just (y + 1, y, y + 1)
  in unfold op 0
