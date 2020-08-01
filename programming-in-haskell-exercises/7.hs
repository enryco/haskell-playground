import Data.Char

------------------------------
-- 7 Higher-order functions --
------------------------------

-- 1
-- map f (filter p xs)
-- test = map f . filter p

-- 2
all' f [] = True
all' f (x:xs) = f x && all' f xs
-- all p = and . map p

any' f [] = False
any' f (x:xs) = f x || any' f xs
-- any p = or . map p

takeWhile' f [] = []
takeWhile' f (x:xs)
  | f x = x:takeWhile f xs
  | otherwise = []

dropWhile' f [] = []
dropWhile' f (x:xs)
  | f x = dropWhile f xs
  | otherwise = xs

-- 3
map' f = foldr (\x y -> f x : y) []
filter' p = foldr filterAppend [] where
  filterAppend x y 
    | p x = x : y
    | otherwise = y
  
filter'' p = foldr (\x y -> if p x then x:y else y) []


-- 4
-- [1,2,3]
-- 
-- dec2int xs = foldl (+) 0 [xi*10^(-n) | (xi, n) <- zip xs [(1-length xs)..0]]
dec2int :: Num a => [a] -> a
dec2int xs = foldl (\x y -> 10*x + y) 0 xs


-- 5
curryWurst :: ((a,b) -> c) -> a -> b -> c
curryWurst = \f -> \x -> \y -> f (x,y)

unCurryWurst :: (a -> b -> c) -> ((a,b) -> c)
unCurryWurst = \f -> \(x,y) -> f x y

-- 6
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 = unfold (\xs -> length xs < 8) (take 8) (drop 8)

map'' f = unfold (== []) (f . head) (tail)

myIterate = unfold (\_ -> False) (id)

-- 7
make8 bits = take 8 (bits ++ repeat 0)
encode = concat . map (make8 . int2bin . ord)
type Bit = Int
bin2int :: Num a => [a] -> a
bin2int bits = sum [w*b | (w,b) <- zip (iterate (*2) 1) bits]

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8