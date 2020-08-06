import Data.List
import Control.Concurrent

type Pos        = (Int,Int)
type GridSize   = Pos
type Colony     = [Pos]


sample_grid  = [(1,1),(0,3),(1,2),(2,2),(1,3),(0,1)] :: Colony
sample_grid2 = [(1,1),(1,2), (2,1), (1,3),(0,2)] :: Colony
sample_grid3 = [(x,5) | x <- [0..9]] :: Colony
glider       = [(3,2), (4,3), (5,3),(4,4),(3,4)] :: Colony

------------------
-- COMPUTATIONS --
------------------

inc :: Int -> Int
inc = (+1)

putBack :: GridSize -> Pos -> Pos
putBack (width, height) (x,y) = ((x + width) `mod` width, (y + height) `mod` height)

dontFallOffTheGrid :: GridSize -> [Pos] -> [Pos]
dontFallOffTheGrid gridSize = map (putBack gridSize)

getNextToPos :: Pos -> [Pos]
getNextToPos (x,y) = [(x+offset_x,y+offset_y) | 
  offset_x <- [-1..1],
  offset_y <- [-1..1],
  (offset_x,offset_y) /= (0,0)]


potentialsOf :: Colony -> Colony
potentialsOf []     = []
potentialsOf colony =  getNextToPos (head colony) ++ potentialsOf (tail colony)

accumulatedPotentialsOff :: GridSize -> Colony -> [[Pos]]
accumulatedPotentialsOff gridSize colony = group (sort (dontFallOffTheGrid gridSize (potentialsOf colony)))

countPotentials :: [[Pos]] -> [(Pos,Int)]
countPotentials [] = []
countPotentials (ps:pss) = (head ps, length ps):countPotentials pss

birthOrAlive :: Colony -> [(Pos,Int)] -> [Pos]
birthOrAlive _ [] = []
birthOrAlive cs ((p,n):pots) | p `elem` cs && n >= 2 && n <= 3 = p:birthOrAlive cs pots
                             | not (elem p cs) && n == 3       = p:birthOrAlive cs pots
                             | otherwise = birthOrAlive cs pots

nextGen :: GridSize -> Colony -> Colony
nextGen gridSize colony = birthOrAlive colony (countPotentials $ accumulatedPotentialsOff gridSize colony)

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

putFrame :: GridSize -> IO ()
putFrame (width, height)= do
  goto (1,1)
  putStrLn $ "╔" ++ replicate width '═' ++ "╗"
  goto (1,height+2)
  putStrLn $ "╚" ++ replicate height '═' ++ "╝"
  sequence_ [writeat "║" (x,y)| y <- [2..(height+1)], x <- [1,width+2]]

run :: GridSize -> Int -> Colony -> Colony -> IO ()
run gridSize iteration initialColony initialColony' = do
  let (_, height) = gridSize
  cls
  putFrame gridSize
  putGrid initialColony
  goto (1, height + 3)
  putStrLn $ "Iteration: " ++ show iteration ++ " | Gridsize: " ++ show gridSize
  putStrLn $ show initialColony'
  threadDelay (1000000 `div` 60)
  run gridSize (inc iteration) (nextGen gridSize initialColony) initialColony'

initiate :: GridSize -> Colony -> IO ()
initiate gridSize c = run gridSize 0 c c

getGridSize :: String -> GridSize
getGridSize ""  = (10,10)
getGridSize str = read str :: GridSize

----------
-- main --
----------

main :: IO ()
main = do
  putStrLn "Select sample grid [1..4]"
  c <- getChar
  putStrLn "Specify GridSize: (width,height)"
  line <- getLine
  let start = initiate $ getGridSize line
  case c of
    '1' -> start glider
    '2' -> start sample_grid
    '3' -> start sample_grid2
    '4' -> start sample_grid3
    _ -> do putStrLn "Not found"; main