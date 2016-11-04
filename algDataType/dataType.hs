{-# LANGUAGE FlexibleInstances #-}
class TooMany a where
  tooMany :: a->Bool

instance TooMany (Int,String) where
  tooMany (n,s) = n>42

newtype Goats = Goats (Int,String) deriving Show

instance TooMany Goats  where
  tooMany (Goats (n,s)) = n>42


data Person=
  Person { name :: String 
          ,age :: Int}
         deriving (Eq,Show)

