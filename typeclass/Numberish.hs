class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer
  defaultNumber :: a

newtype Age = 
  Age Integer
  deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n
  defaultNumber = Age 65


data DayOfWeek = 
  Mon | Tue | Weds | Thu | Fri | Sat
  deriving(Ord, Show)

data Date = Date DayOfWeek Int
instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) _ _ = False

--instance Ord DayOfWeek where


instance Eq Date where
  (==) (Date wday mNum)
       (Date wday' mNum') =
     wday==wday' && mNum==mNum'


funcZ x =
  case x + 1==1 of 
    True -> "AWESOME"
    False -> "false"

pal xs = 
  case xs == reverse xs of
    True -> "true"
    False -> "false"
