import System.Environment (getArgs)
import Data.List (foldl')


interactWith function inputFile = do
  input <- readFile inputFile
  print (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input] -> interactWith function input
            _ -> putStrLn "error: exactly one argument needed"
        myFunction = transpose

transpose :: String -> String
transpose file = unlines $ foldl' flatzip (repeat "") lins
  where lins = lines file

flatzip :: [String] -> String -> [String]
flatzip xs y = let tuples = zip xs y
                   flatten (x, ypart) = x ++ [ypart]
                in map flatten tuples
