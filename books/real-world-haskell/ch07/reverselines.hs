import System.IO

main :: IO()
main = do
  interact (unlines . reverse . lines)
