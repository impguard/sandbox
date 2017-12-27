import Data.Char (ord, chr, toUpper)

-- Vigenere's Cipher

type Plaintext = String
type Keyword = String

ordA :: Int
ordA = ord 'A'

duplicate :: String -> String
duplicate = concat . repeat

addSpaces :: String -> String -> String
addSpaces [] _ = ""
addSpaces _ [] = ""
addSpaces (x:xs) yall@(y:ys) = if x == ' ' then ' ' : (addSpaces xs yall) else y : (addSpaces xs ys)

removeSpaces :: String -> String
removeSpaces [] = ""
removeSpaces (x:xs) = if x == ' ' then removeSpaces xs else x : removeSpaces xs

cipher :: Plaintext -> Keyword -> String
cipher p k = let order = subtract ordA . ord
                 char = chr . (+ordA)
                 encode = \x y -> mod (x + y) 26
                 plaintext = removeSpaces $ map toUpper p
                 keyword = addSpaces plaintext (duplicate (map toUpper k))
              in addSpaces p $ map char $ zipWith encode (map order plaintext) (map order keyword)

-- As Patterns

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xall@(x:xs) (y:ys) = if x == y then isSubseqOf xs ys else isSubseqOf xall ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords s = let convert xall@(x:xs) = (xall, toUpper x : xs)
                     in map convert (words s)
