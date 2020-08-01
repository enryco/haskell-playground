import Data.Char
import Debug.Trace
import System.Environment

-------------------------------------------------------
-- Find the longest repeating substring using ngrams --
-------------------------------------------------------

countSeq :: Eq a => a -> [a] -> Int
countSeq seq' = length . filter (\xs -> xs == seq')

generateNGram :: Int -> [a] -> [[a]]
generateNGram _ [] = []
generateNGram len (x:xs) | length xs >= (len-1) = (x:take (len-1) xs):generateNGram len xs
                         | otherwise            = []

without :: Eq a => [a] -> a -> [a]
without xs x = [xi | xi <- xs, xi /= x]

countNGrams :: Eq a => [a] -> [(a, Int)]
countNGrams [] = []
countNGrams (x:xs) = (x, (countSeq x xs)+1):countNGrams (xs `without` x)

filterSingleCountedOccurences :: [(a, Int)] -> [(a, Int)]
filterSingleCountedOccurences = filter (\(_,i) -> i > 1)

qsortNGrams :: [(a, Int)] -> [(a, Int)]
qsortNGrams [] = []
qsortNGrams ((x,i):xs) = higher ++ [(x,i)] ++ lower where
  lower  = qsortNGrams (filter (\(x,ii) -> ii <= i) xs)
  higher = qsortNGrams (filter (\(x,ii) -> ii > i) xs)

getNGramCountOfLength :: Eq a => Int -> [a] -> [([a], Int)]
getNGramCountOfLength len =  filterSingleCountedOccurences . countNGrams . generateNGram len

getHighestNGramCountWithLength :: Eq a => Int -> [a] -> [([a],Int)]
getHighestNGramCountWithLength len xs
  | trace ("length " ++ show len) False = undefined
  | res == []                           = getHighestNGramCountWithLength (len-1) xs
  | otherwise                           = res where
    res = getNGramCountOfLength len xs

getLongestSequence :: Eq a => [a] -> [([a],Int)]
getLongestSequence seq' = getHighestNGramCountWithLength (length seq' `div` 2) seq'

getNGramsMinMax :: Eq a => Int -> Int -> [a] -> [[([a], Int)]]
getNGramsMinMax minLen maxLen xs = [getNGramCountOfLength len xs | len <- [minLen..maxLen]]

--------------------------------
-- Calculate pirates salaries --
--------------------------------

positions x xs = [i | (xi, i) <- zip xs [0..], x == xi]
sumPirateSalarie pirate pirates salaries = sum (map (salaries !!) (positions pirate pirates))
piratesSalarieSums pirates salaries = [(sumPirateSalarie pirate pirates salaries, pirate) | pirate <- pirates]


----------
-- main --
----------


main :: IO ()
main = do
  args <- getArgs
  putStrLn (show (getLongestSequence (head args)))
