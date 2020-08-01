import Data.List
import Control.Concurrent

type Pos = (Int,Int)
type Colony = [Pos]


grid_size = 60 :: Int

sample_grid   = [(1 :: Int,1 :: Int),(0,3),(1,2),(2,2),(1,3),(0,1)]
sample_grid2  = [(1 :: Int,1 :: Int),(1,2), (2,1), (1,3),(0,2)]
sample_grid3  = [(x :: Int,5 :: Int) | x <- [0..9]]
glider        = [(3 :: Int,2 :: Int), (4,3), (5,3),(4,4),(3,4)]

------------------
-- COMPUTATIONS --
------------------

inc :: Int -> Int
inc = (+1)

dontFallOffTheGrid :: Pos -> Pos
dontFallOffTheGrid (x,y) = (putBackInside x, putBackInside y) where
  putBackInside coordinate = (coordinate + grid_size) `mod` grid_size


getNextToPos :: Pos -> Colony
getNextToPos (x,y) = [dontFallOffTheGrid (x+offset_x,y+offset_y) | 
  offset_x <- [-1..1],
  offset_y <- [-1..1],
  (offset_x,offset_y) /= (0,0)]


potentialsOf :: Colony -> Colony
potentialsOf []     = []
potentialsOf colony =  getNextToPos (head colony) ++ potentialsOf (tail colony)

accumulatedPotentialsOff :: Colony -> [[Pos]]
accumulatedPotentialsOff colony = group (sort (potentialsOf colony))

countPotentials :: [[Pos]] -> [(Pos,Int)]
countPotentials [] = []
countPotentials (ps:pss) = (head ps, length ps):countPotentials pss

birthOrAlive :: Colony -> [(Pos,Int)] -> [Pos]
birthOrAlive _ [] = []
birthOrAlive cs ((p,n):pots) | p `elem` cs && n >= 2 && n <= 3 = p:birthOrAlive cs pots
                             | not (elem p cs) && n == 3       = p:birthOrAlive cs pots
                             | otherwise = birthOrAlive cs pots

nextGen :: Colony -> Colony
nextGen colony = birthOrAlive colony (countPotentials (accumulatedPotentialsOff colony))

----------------
-- IO ACTIONS --
----------------

putGrid :: Colony -> IO ()
putGrid colony = sequence_ (map (writeatWithOffset (2,2) "▒") colony)

cls :: IO ()
cls = putStr "\ESC[2J"

writeat :: String -> Pos -> IO ()
writeat xs p = do
  goto p
  putStr xs

writeatWithOffset :: Pos -> String -> Pos -> IO ()
writeatWithOffset (ox, oy) xs (x,y) = writeat xs (x+ox, y+oy)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H" )

putFrame :: IO ()
putFrame = do
  goto (1,1)
  putStrLn $ "╔" ++ replicate grid_size '═' ++ "╗"
  goto (1,grid_size+2)
  putStrLn $ "╚" ++ replicate grid_size '═' ++ "╝"
  sequence_ [writeat "║" (x,y)| y <- [2..(grid_size+1)], x <- [1,grid_size+2]]

run :: Int -> Colony -> Colony -> IO ()
run iteration initialColony initialColony' = do
  cls
  putFrame
  putGrid initialColony
  goto (1, grid_size + 3)
  putStrLn $ "Iteration: " ++ show iteration ++ " | Gridsize: " ++ show grid_size
  putStrLn $ show initialColony'
  threadDelay 100000
  run (inc iteration) (nextGen initialColony) initialColony'

start :: Colony -> IO ()
start c = run 0 c c

----------
-- main --
----------

main :: IO ()
main = do
  putStrLn "Select sample grid [1..4]"
  c <- getChar
  case c of
    '1' -> start glider
    '2' -> start sample_grid
    '3' -> start sample_grid2
    '4' -> start sample_grid3
    _ -> do putStrLn "Not found"; main