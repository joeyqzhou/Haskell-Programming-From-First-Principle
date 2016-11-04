printInc n = print plusTwo 
  where plusTwo = n + 2

mult1 = x * y
  where x = 5
        y = 6

fa :: String -> String 
fa a = a ++ "!"

fb :: String -> Char
fb a = a !! 4

rv :: String->String
rv (x:xs) = (rv xs) ++ [x]
rv [] = []


main :: IO()
main = print $ rv "hello world"
