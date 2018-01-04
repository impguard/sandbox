module Hello
  ( sayHello
  ) where

import           Control.Monad
import           Data.Char     (isPunctuation, isSeparator, toLower)
import           System.Exit   (exitSuccess)

sayHello :: String -> IO ()
sayHello name = putStrLn ("Hi " ++ name ++ "!")

-- 13.14 Exercises
-- 2.
palindrome :: IO ()
palindrome =
  forever $ do
    line1 <- getLine
    if line1 == reverse line1
      then putStrLn "It's a palindrome!"
      else do
        putStrLn "Nope!"
        exitSuccess

-- 3.
toLowerWord :: String -> String
toLowerWord = fmap toLower

removeSpecial :: String -> String
removeSpecial = filter (not . isSpecial)
  where
    isSpecial c = isPunctuation c || isSeparator c

palindrome' :: IO ()
palindrome' =
  forever $ do
    line <- getLine
    word <- return $ (removeSpecial . toLowerWord) line
    if word == reverse word
      then putStrLn "It's a palindrome!"
      else do
        putStrLn "Nope!"
        exitSuccess
