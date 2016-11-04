main1 :: IO Bool
main1 = do c <- getChar
           c'<- getChar
           return (c == c')

main :: IO()
main = do c <- getChar
          c'<- getChar
          if c == c'
          then putStrLn "right"
          else putStrLn "false"
