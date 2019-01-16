import Data.Char (digitToInt, isDigit)
import Data.List (foldl', isPrefixOf)

asIntError xs
  | null xs = error "empty string passed"
  | "-" `isPrefixOf` xs = (negate . asIntError . tail) xs
  | otherwise = foldl' step 0 xs
  where step accum x
          | maxBound `div` 10 < accum = error "max integer value reached"
          | isDigit x = accum * 10 + digitToInt x
          | otherwise = error "invalid digit"


type ErrorMessage = String

asIntSafe :: String -> Either ErrorMessage Int
asIntSafe xs
  | null xs = Left "empty string"
  | "-" `isPrefixOf` xs = case (asIntSafe . tail) xs of
                           Right value -> Right (-value)
                           error       -> error
  | otherwise = foldl' step (Right 0) xs
  where step (Right accum) x
          | maxBound `div` 10 < accum = Left "number too large"
          | isDigit x = Right $ accum * 10 + digitToInt x
          | otherwise = Left ("non-digit'" ++ [x] ++ "'")
        step error _ = error


-- Just understanding why foldl flips arguments compared to foldr The gist is
-- that it doesn't need to, as the below definition demonstrates.  However,
-- it's done this way to achieve mathemematical consistency. So that when I
-- have a left associative left fold it looks like:
--
--   foldl step zero [x, y, z] == (((zero + x) + y) + z)
--
-- Whereas if I define it like I did below it looks like:
--
--   myfoldl step zero [x, y, z] == (((x + zero) + y) + z)
--
-- which simply looks funnier when explaining.

myfoldl :: (b -> a -> a) -> a -> [b] -> a
myfoldl _ zero [] = zero
myfoldl step zero (x:xs) = myfoldl step (step x zero) xs
