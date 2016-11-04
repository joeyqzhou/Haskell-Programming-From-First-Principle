import Data.List (intercalate)
import Data.List

f1 :: Integer -> Maybe Integer
f1 n = if even n then Just (n+2) else Nothing


type Name = String
type Age = Integer
type VlidatePerson a = Either [InvalidPerson] a

data Person = Person Name Age deriving Show

mkPerson:: Name->Age->Maybe Person
mkPerson name age 
  | name /= "" && age>0 = Just $ Person name age
  | otherwise  = Nothing




data InvalidPerson = EmptyName | NotAge deriving (Show,Eq)

mkPerson':: Name->Age->Either InvalidPerson Person
mkPerson' name age
  | name =="" = Left EmptyName
  | age <= 0 = Left NotAge
  | otherwise = Right $ Person name age
 

ageOK :: Age -> VlidatePerson Age
ageOK age 
  | age>0 = Right age
  | otherwise = Left [NotAge]

nameOK :: Name -> VlidatePerson Name
nameOK name 
  | name =="" = Left [EmptyName]
  | otherwise = Right name

mkPerson'' ::  VlidatePerson Name 
            -> VlidatePerson Age
            ->  VlidatePerson Person
mkPerson'' (Right name) (Right age) = Right $ Person name age
mkPerson'' (Left name) (Left age) = Left (name++age)
mkPerson'' (Left name) _ = Left name
mkPerson'' _ (Left age) = Left age

mkPerson''' :: Name->Age-> VlidatePerson Person
mkPerson''' name age = mkPerson'' (nameOK name) (ageOK age)




notThe :: String -> Maybe String
notThe "the" = Nothing
notThe a = Just a

replaceThe :: String->String
replaceThe s =  intercalate " " $map athe $fmap notThe $ words s
  where athe Nothing = "a"
        athe (Just str) = str



isVowel :: Char -> Bool
isVowel x = x `elem` "aeiou"

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

mkWord ::String->Maybe Word'
mkWord w = if lv < lnv then Just (Word' w) else Nothing
  where lv = length $ filter (\i -> elem i vowels) w
        lnv = length w - lv


data Nat = 
  Zero
 |Succ Nat
 deriving (Eq,Show)

natToInterger ::Nat -> Integer
natToInterger Zero = 0
natToInterger (Succ nat) = 1 + natToInterger nat

integerToNat :: Integer -> Maybe Nat 
integerToNat a 
  | a>0 = Just (itn a)
  | otherwise = Nothing
  where itn a 
           | a>0 = Succ (itn (a-1))
           | a==0 = Zero




isNothing :: Maybe a-> Bool
isNothing Nothing = True
isNothing _ = False

isJust = not . isNothing

mayybee::b->(a->b)->Maybe a->b
mayybee b f Nothing = b
mayybee _ f (Just a) = f a


fromMaybe :: a-> Maybe a -> a
fromMaybe a b = mayybee a id  b 

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]


catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs)  = catMaybes xs
catMaybes ( (Just a):xs) = a:(catMaybes xs)


flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe xs = foldr f (Just []) xs
  where f _ Nothing = Nothing
        f Nothing _ = Nothing
        f (Just y) (Just x) = Just (y:x)

lefts' :: [Either a b] -> [a]
lefts' es = foldr f [] es
  where f (Left x) xs = (x:xs)
        f (Right _) xs = xs

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where f (Left x) xs = xs
        f (Right x) xs = (x:xs)

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr f ([],[])
  where f (Left x) (xs,ys) = ((x:xs),ys)
        f (Right y) (xs,ys) = (xs,(y:ys))


eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left a) = Nothing
eitherMaybe' f (Right b) = Just (f b)

either'::(a->c)->(b->c)->Either a b->c
either' f g (Left a) = f a
either' f g (Right b) = g b



