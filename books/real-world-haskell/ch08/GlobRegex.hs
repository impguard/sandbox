module GlobRegex
    (
      globToRegex
    , matchesGlob
    ) where

import Text.Regex.Posix ((=~))

type GlobError = String

-- Core globToRegex functionality

try :: (a -> a) -> Either b a -> Either b a
try func value = case value of
                   Right result -> Right $ func result
                   error        -> error

globToRegex :: String -> Either GlobError String
globToRegex cs = try handler (globToRegex' cs)
  where handler pat = '^' : pat ++ "$"

globToRegex' :: String -> Either GlobError String
globToRegex' "" = Right ""

globToRegex' ('*':cs) = try handler (globToRegex' cs)
  where handler pat = ".*" ++ pat

globToRegex' ('?':cs) = try handler (globToRegex' cs)
  where handler pat = '.' : pat

globToRegex' ('[':'!':c:cs) = try handler (charClass cs)
  where handler charClass' = "[^" ++ c : charClass'
globToRegex' ('[':c:cs)     = try handler (charClass cs)
  where handler charClass' = "[" ++ c : charClass'
globToRegex' ('[':_)        = Left "unterminated character class"

globToRegex' (c:cs) = try handler (globToRegex' cs)
  where handler pat = escape c ++ pat

-- Helpers

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
  where regexChars = "\\+()^$.{}]"

charClass :: String -> Either GlobError String
charClass (']':cs) = try handler (globToRegex' cs)
  where handler pat = ']' : pat
charClass (c:cs)   = try handler (charClass cs)
  where handler chars = c : chars
charClass []       = Left "unterminated character class"

matchesGlob :: FilePath -> String -> Either GlobError Bool
name `matchesGlob` pat = case globToRegex pat of
                           Right regex  -> Right $ name =~ regex
                           Left error -> Left error
