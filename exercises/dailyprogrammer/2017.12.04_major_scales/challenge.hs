import Text.Read (readMaybe)
import System.Environment (getArgs)
import Control.Monad (liftM2, join)

data Note = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B deriving (Read, Show, Enum)
data Solfege = Do | Re | Mi | Fa | So | La | Ti deriving (Read, Show)

solfegeToNumber :: Solfege -> Int
solfegeToNumber n = case n of
    Do -> 0
    Re -> 2
    Mi -> 4
    Fa -> 5
    So -> 7
    La -> 9
    Ti -> 11

toSolfege :: String -> Maybe Solfege
toSolfege s = readMaybe s

toNote :: String -> Maybe Note
toNote s = case readMaybe s of
    Just note -> return note
    Nothing   -> case s of
        "C#" -> Just Cs
        "D#" -> Just Ds
        "E#" -> Just F
        "F#" -> Just Fs
        "G#" -> Just Gs
        "A#" -> Just As
        "B#" -> Just B
        _    -> Nothing

fromNote :: Note -> String
fromNote n = case n of
    Cs -> "C#"
    Ds -> "D#"
    Fs -> "F#"
    Gs -> "G#"
    As -> "A#"
    _ -> show n

getNote :: Note -> Solfege -> Note
getNote note solfege = let nn = fromEnum note
                           sn = solfegeToNumber solfege
                           in toEnum $ (nn + sn) `rem` 12

getNote' :: String -> String -> Maybe Note
getNote' note' solfege' = do
    note <- toNote note'
    solfege <- toSolfege solfege'
    return $ getNote note solfege

(<!!>) :: [a] -> Int -> Maybe a
(<!!>) [] _ = Nothing
(<!!>) (x:xs) n
    | n == 0    = return x
    | n < 0     = Nothing
    | otherwise = xs <!!> (n - 1)

main = do
    args <- getArgs
    result <- return $ join $ liftM2 getNote' (args <!!> 0) (args <!!> 1)
    case result of
        Nothing -> putStrLn "Usage: ./challenge.hs NOTE SOLFEGE\n\n\
                            \Determines the note that is the solfege of the note passed in.\n\
                            \The note must be a valid note (sharps only). The solfege must\n\
                            \be a valid solfege."
        Just note -> putStrLn $ fromNote note
