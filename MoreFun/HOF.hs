data Employee = Coder 
               | Manager
               | CEO
               deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO()
reportBoss e e' =
  putStrLn $ show e ++ " is the boss of " ++ show e'

coderRule :: Employee -> Employee -> Ordering
coderRule Coder Coder = EQ
coderRule Coder _ = GT
coderRule _ Coder = LT
coderRule e e' = compare e e'

employeeRank :: (Employee -> Employee -> Ordering)
               -> Employee
               -> Employee
               -> IO()

employeeRank f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "equal"
    LT -> (flip reportBoss) e e'


bloodNa :: Integer -> String
bloodNa x 
  | x<135 = "too low"
  | x>145 = "too high"
  | otherwise = "just right"

add :: Int -> Int -> Int
add a b = a + b

add' :: Int -> Int -> Int
add' = (+)


addOne :: Int -> Int
addOne a = a + 1

addOne' :: Int -> Int
addOne' = (+ 1)

main :: IO()
main = do
  print (0 :: Int)
  print (add 1 0)
  print (addOne 0)
  


