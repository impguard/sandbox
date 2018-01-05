module Main
  ( main
  ) where

import           Chapter15

main :: IO ()
main = do
  putStrLn $ "hello" ++ world
  putStrLn "stop complaining!"
