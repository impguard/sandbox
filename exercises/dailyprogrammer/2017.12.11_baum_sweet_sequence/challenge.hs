import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

binary :: Integer -> String
binary n = showIntAtBase 2 intToDigit n ""

split1 :: String -> [String]
split1 [] = [""]
split1 (x:xs)
    | x == '1'  = if head rest == "" then rest else "" : rest
    | otherwise = (x : head rest) : tail rest
    where rest = split1 xs

showbn :: [Integer] -> String
showbn [] = ""
showbn (x:[]) = show x
showbn (x:xs) = show x ++ ", " ++ showbn xs

bn :: Integer -> [Integer]
bn n = map bn' [0 .. n]

bn' :: Integer -> Integer
bn' n
    | n == 0 = 1
    | all (even . length) (split1 $ binary n) = 1
    | otherwise = 0

(<!!>) :: [a] -> Integer -> Maybe a
(<!!>) [] n = Nothing
(<!!>) (x:xs) n
    | n == 0    = return x
    | n < 0     = Nothing
    | otherwise = (<!!>) xs (n - 1)

main = do
    args <- getArgs
    mn <- return (args <!!> 0 >>= readMaybe :: Maybe Integer)
    n <- case mn of
        Nothing -> do
            putStrLn "Usage: ./challenge.hs NUMBER \n\n\
                     \Computes the Baum-Sweet sequence from 0 to NUMBER."
            exitFailure
        Just n -> return n
    putStrLn $ showbn (bn n)
