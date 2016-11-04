module Jammin where
import Data.List (sortBy, groupBy,maximumBy)
import Data.Char

data Fruit =
  Peach
  | Plum
  | Apple
  | Blackberry deriving (Eq, Show,Ord)

data JamJars = 
  Jam { fruit :: Fruit, jars::Int} deriving (Eq,Show)



row1 = Jam {fruit = Plum, jars = 4} -- construct using record syntax
row2 = Jam Peach 1
row3 = Jam Plum 4
row4 = Jam Blackberry 8
row5 = Jam Apple 4
row6 = Jam Apple 7
allJam = [row1, row2, row3, row4, row5, row6]

rowJars :: [JamJars] -> [Int]
rowJars = map jars

jarsCount :: [JamJars] -> Int
jarsCount = sum . rowJars

mostRow :: [JamJars]->JamJars
mostRow = maximumBy (\j1 j2 -> compare (jars j1) (jars j2))

sortByRow :: [JamJars]->[JamJars]
sortByRow = sortBy (\j1 j2 -> compare (jars j1) (jars j2))


groupJam :: [JamJars] -> [[JamJars]]
groupJam = groupBy (\j1 j2 -> j1==j2) . sortByRow

data Product a b =
  a :&: b
  deriving (Eq,Show)


data BinaryTree a =
	Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insertBinaryTree :: (Num a,Ord a) => a-> BinaryTree a -> BinaryTree a
insertBinaryTree b Leaf = Node Leaf b Leaf
insertBinaryTree b (Node left a right)
  | b==a =Node left a right
  | b<a =Node (insertBinaryTree b left) a right
  | b>a =Node left a (insertBinaryTree b right)

mapBinaryTree :: (a->b) -> BinaryTree a ->  BinaryTree b
mapBinaryTree f Leaf = Leaf
mapBinaryTree f (Node left a right) = Node (mapBinaryTree f left) (f a) (mapBinaryTree f right)

preorder2list :: BinaryTree a -> [a]
preorder2list Leaf = []
preorder2list (Node left a right) =  a:(preorder2list left ++ preorder2list right)

inorder2list :: BinaryTree a -> [a]
inorder2list  Leaf = []
inorder2list (Node left a right) = (inorder2list left) ++ [a] ++ (inorder2list right)


foldTree :: (a->b->b->b) ->b -> BinaryTree a ->b
foldTree _ acc Leaf = acc
foldTree f acc (Node left a right) = f a (foldTree f acc left) (foldTree f acc right)

mapTree' :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree' f bt = foldTree mk Leaf bt
  where mk a l r =  Node l (f a) r



 
data Quantum =
   Yes
 | No
 | Both
 deriving (Eq,Show)

quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes

quantSum2 :: Either Quantum Quantum
quantSum2 = Right No


quantSum3 :: Either Quantum Quantum
quantSum3 = Right Both
quantSum4 :: Either Quantum Quantum 
quantSum4 = Left Yes


isSubseqOf :: (Eq a)=> [a]->[a]->Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xs ys = g [] xs ys
  where g :: (Eq a)=> [a]->[a]->[a]->Bool
        g preSave [] _ = True
        g preSave _ [] = False
        g preSave (x:xs) (y:ys)
          | x==y = g (preSave++[x]) xs ys
          | otherwise = g [] (preSave++(x:xs)) ys


capitalize :: String->String
capitalize [] = [] 
capitalize (x:xs) = (toUpper x):xs




--isSubseqOf (x:xs) (y:ys)   
--  | x==y = isSubseqOf xs ys
--  | otherwise = isSubseqOf (x:xs) ys 





