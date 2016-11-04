module Main where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1+1 > 1" $ do
      (1+1)>1 `shouldBe` True
    it "2 + 2 ==4" $ do
      (2+2) == 4 `shouldBe` True
    it " 15 / 3 = 5" $ do
      dividedBy 15 3 `shouldBe` (5,0)
    it " multiply 2 8 = 16" $ do
      multiply' 2 8 `shouldBe` 16
    it " x+1 > x " $ do
      property $ \x -> x + 1 > (x::Int)


dividedBy :: Integral a => a -> a -> (a, a) 
dividedBy num denom = go num denom 0
  where go n d count 
         | n < d = (count,n)
         | otherwise = go (n-d) d (count+1)
--can not be negative
multiply'::(Eq a, Num a,Ord a) => a->a->a
multiply' _ 0 = 0
multiply' 0 _ = 0
multiply' x y = x +  multiply' x (y-1)

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]


genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a,b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a,b)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

addTest :: Int -> Bool
addTest x = x + 1 > x

runTest :: IO()
runTest = quickCheck addTest
