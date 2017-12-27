import Data.Char (toLower)
import Data.List (elemIndex)
import Data.Map (fromListWith, toList)

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

data DaPhone = DaPhone [(Digit, [Char])] deriving (Show)

phone :: DaPhone
phone = DaPhone [('1', "1"),
                 ('2', "abc2"),
                 ('3', "def3"),
                 ('4', "ghi4"),
                 ('5', "jkl5"),
                 ('6', "mno6"),
                 ('7', "pqrs7"),
                 ('8', "tuv8"),
                 ('9', "wxyz9"),
                 ('*', "^*"),
                 ('0', " 0"),
                 ('#', ".,#")]

convo :: [String]
convo =
       ["Wanna play 20 questions",
        "Ya",
        "U 1st haha",
        "Lol ok. Have u ever tasted alcohol",
        "Lol ya",
        "Wow ur cool haha. Ur turn",
        "Ok. Do u think I am pretty Lol",
        "Lol ya",
        "Just making sure rofl ur turn"]

(<!!>) :: DaPhone -> Char -> (Digit, [Char])
(<!!>) (DaPhone p) c = go p c
    where go (x:xs) c = if c `elem` snd x then x else go xs c

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps p c = let lc = toLower c
                      (digit, directory) = p <!!> lc
                      presses = case elemIndex lc directory of
                        Just index -> index
                      result = (digit, presses + 1)
                   in if lc == c then [result] else [('*', 1), result]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead p s = concat $ map (reverseTaps p) s

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps [] = 0
fingerTaps (x:xs) = (snd x) + fingerTaps xs

removeSpaces :: String -> String
removeSpaces [] = ""
removeSpaces (x:xs) = if x == ' ' then removeSpaces xs else x : removeSpaces xs

mostPopularLetter :: String -> Char
mostPopularLetter s = let folder n a = if snd a > snd n then a else n
                       in fst $ foldr folder ('a', 0) (toList $ fromListWith (+) [(c, 1) | c <- removeSpaces s])

mostPopularWord :: String -> String
mostPopularWord s = let folder n a = if snd a > snd n then a else n
                     in fst $ foldr folder ("a", 0) (toList $ fromListWith (+) [(w, 1) | w <- words s])

mostPopularLetterWithCost :: DaPhone -> String -> (Char, Presses)
mostPopularLetterWithCost p s = let letter = mostPopularLetter s
                                 in (letter, fingerTaps . cellPhonesDead p . filter (==letter) $ s)

coolestLtr :: [String] -> Char
coolestLtr strings = mostPopularLetter . concat $ strings

coolestWord :: [String] -> String
coolestWord strings = mostPopularWord . unwords $ strings
