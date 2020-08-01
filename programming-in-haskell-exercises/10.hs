import Data.Char

newline :: IO ()
newline = putChar '\n'

getNat :: String -> IO Int
getNat prompt = do  putStr prompt
                    xs <- getLine
                    if all isDigit xs then
                      return (read xs)
                    else
                      do putStrLn "ERROR: Invalid digit"
                         getNat prompt


totalRemain :: Int -> Int -> IO ()
totalRemain total 0 = putStrLn ("The total is " ++ show total)
totalRemain total r = do next <- getNat ""
                         totalRemain (total+next) (r-1)

-- adder :: IO ()
-- adder = do r <- getNat "How many numbers?"
--            totalRemain 0 r

adder' :: IO ()
adder' = do r <- getNat "How many numbers? "
            digits <- sequence (replicate r (getNat ""))
            putStrLn (show (sum digits))


-- 6 --
readLine :: IO String
readLine  = do  c <- getChar
                case c of
                  '\n' -> return ""
                  '\DEL' -> do putStr "\b\b\ESC[3~\ESC[3~"
                               l <- readLine
                               return ('\b':l)
                  _ -> do l <- readLine
                          return (c:l)
  


main :: IO ()
main = do   
    line <- getLine  
    if null line  
        then return ()  
        else do  
            putStrLn $ reverseWords line  
            main  
  
reverseWords :: String -> String  
reverseWords = unwords . map reverse . words  