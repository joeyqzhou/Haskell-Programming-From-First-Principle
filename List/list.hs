import Data.Bool
myEnumFromTo :: (Ord a, Enum a)=> a->a->[a]
myEnumFromTo x y 
  | x > y = []
  | otherwise = x:myEnumFromTo (succ x) y

myWords :: String -> [String]
myWords [] = []
myWords (' ':xs) = myWords xs
myWords x = takeWhile (/=' ') x : myWords (dropWhile (/=' ') x)

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x==True then True else myOr xs

myAny :: (a->Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs)
  | f x = True
  | otherwise = myAny f xs

myAny' :: (a->Bool) -> [a] -> Bool
myAny' f [] = False
myAny' f (x:xs) = f x || myAny' f xs

myElem :: Eq a => a->[a]->Bool
myElem _ [] = False
myElem z (x:xs) 
  | z==x = True
  | otherwise = myElem z xs

myReverse :: [a]->[a]
myReverse [] = []
myReverse xs = go [] xs
  where go ys [] = ys
        go ys (x:xs) = go (x:ys) xs


squish:: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a->[b])->[a]->[b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squish':: [[a]] -> [a]
squish' xs =  squishMap id xs




myMaximumBy :: (a->a->Ordering) -> [a] -> a
myMaximumBy _ [] = undefined
myMaximumBy _ (x:[]) = x
myMaximumBy f (x:xs) = go f x xs
  where go f b (x:xs)
            | f b x == GT = go f b xs
            | otherwise = go f x xs
        go f b [] = b

myMaximum :: (Ord a) => [a] -> a
myMaximum xs = myMaximumBy (\a b -> compare a b) xs 
