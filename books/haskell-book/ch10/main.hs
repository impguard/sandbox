{-# LANGUAGE GeneralizedNewtypeDeriving #-}


class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42


newtype Twople = Twople (Int, String)

instance TooMany Twople where
  tooMany (Twople (n, _)) = tooMany n


newtype Twoint = Twoint (Int, Int)

instance TooMany Twoint where
  tooMany (Twoint (n, m)) = tooMany (n + m)


newtype Twoweird a = Twoweird (a, a)

instance (Num a, TooMany a) => TooMany (Twoweird a) where
  tooMany (Twoweird (n, m)) = tooMany (n + m)
