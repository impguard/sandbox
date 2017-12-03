import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
                                    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
                                    Just (state, code) -> if state /= Taken
                                                            then Right code
                                                            else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]


data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

leaf :: a -> Tree a
leaf x = Node x Empty Empty

treeInsert :: (Ord a) =>  a -> Tree a -> Tree a
treeInsert x Empty = leaf x
treeInsert x (Node a left right) =
    if x <= a
    then Node a (treeInsert x left) right
    else Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x Empty = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right


data TrafficLight = Red | Yellow | Green
instance Show TrafficLight where
    show Red = "Red Light! Stop!"
    show Green = "Green Light! Go!"
    show Yellow = "Woooooh....slow down!"

instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)
