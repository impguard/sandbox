splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith pred (x:xs)
  | pred x       = case splitWith pred xs of
                    (word:words) -> (x:word):words
                    []           -> [[x]]
  | not (pred x) = [[]] ++ splitWith pred xs
splitWith _ [] = []
