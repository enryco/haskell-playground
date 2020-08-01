import Data.Char

-- Tinkering around with the bind operator

appendToLine :: a -> [a] -> [a]
appendToLine x xs = x:xs

-- getLine' :: [Char] ->  IO ()
-- getLine' xs = do
--   c <- getChar
--   if c == '\n' then
--     putStr (xs)
--   else
--     getLine' (xs ++ [c])


getLine' :: [Char] ->  IO String
getLine' xs = getChar >>= \c -> 
  if c == '\n' then
    return (xs)
  else
    getLine' (xs ++ [c])

getLine'' :: IO String
getLine'' = getLine' []


getLine''' :: IO String
getLine''' = getChar >>= \c ->
  if c == '\n' then
    return []
  else
    getLine''' >>= \d -> return (c:d)


main :: IO ()
main = getChar >>= \c -> 
       getChar >>= \d ->
         putStr (c:d:[]) 