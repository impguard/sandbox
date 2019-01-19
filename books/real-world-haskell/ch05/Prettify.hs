module Prettify
  (
  Doc,
  char,
  text,
  double,
  empty,
  tab,
  (</>),
  (<.>),
  hcat,
  fsep,
  punctuate,
  compact,
  pretty,
  line,
  softline,
  flatten,
  ) where


data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Tab
         | Untab
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show, Eq)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

tab :: Doc
tab = Tab

(<.>) :: Doc -> Doc -> Doc
Empty <.> y = y
x <.> Empty = x
x <.> y = x `Concat` y

hcat :: [Doc] -> Doc
hcat = fold (<.>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <.> softline <.> y

softline :: Doc
softline = (char ' ') `Union` line

{--

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

--}


punctuate :: Doc -> Doc -> [Doc] -> [Doc]
punctuate begin end [] = []
punctuate begin end [d] = [begin <.> d]
punctuate begin end (d:ds) = (begin <.> d <.> end): (punctuate begin end ds)

flatten :: Doc -> [Doc]
flatten d = case d of
              Empty        -> []
              a `Concat` b -> flatten a ++ flatten b
              a            -> [a]

compact :: Doc -> String
compact x = transform [x]
  where transform [] = ""
        transform (d:ds) =
          case d of
            Empty        -> transform ds
            Tab          -> transform ds
            Char c       -> c : transform ds
            Text s       -> s ++ transform ds
            Line         -> transform ds
            a `Concat` b -> transform (a:b:ds)
            _ `Union` b  -> transform (b:ds)

pretty :: Int -> Int -> Doc -> String
pretty width tabw x = best 0 True [x]
  where best col is_start (d:ds) =
          case d of
            Empty        -> best col is_start ds
            Char c       -> c : best (col + 1) False ds
            Tab          -> let next = best col False ds
                                space = if is_start then replicate tabw ' ' else ""
                             in space ++ next
            Text s       -> s ++ best (col + length s) False ds
            Line         ->  '\n' : best 0 True ds
            a `Concat` b -> best col is_start (a:b:ds)
            a `Union` b  -> nicest col (best col is_start (a:ds))
                                       (best col is_start (b:ds))
        best _ _ _ = ""
        nicest col a b | (width - least) `fits` a = a
                       | otherwise                = b
                       where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs
