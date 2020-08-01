import Data.Char


-- some test functions
double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

a = b + c
  where 
    b = 1
    c = 2

d = a * 2

n = div a (length ns)
  where
    a = 10
    ns = [1,2,3,4,5]

myLast1 xs = xs !! (length xs - 1)

myLast2 [xs] = xs
myLast2 (x:xs) = myLast2 xs

myLast3 xs = head (reverse xs)


init1 xs = take (length xs -1) xs
init2 xs = reverse (tail (reverse xs))


add' :: Int -> Int -> Int
add' x y = x+y

-- 3.11 Exercises
second xs = head (tail xs)
swap (x,y) = (y,x)
pair x y = (x,y)
double' x = x*2
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

signum' n | n < 0 = 1
          | n == 0 = 0
          | otherwise = 1

-- 4.8 1. halve
halve :: [a] -> ([a],[a])
halve xs = (take hl xs, drop hl xs) where
  hl = length xs `div` 2

-- 4.8 2. third
third1 xs = head (tail (tail xs))
third2 xs = xs !! 2
third3 (_:_:x:_) = x


-- 4.8 3. safetails
st_a xs = if null xs then [] else tail xs

st_b xs | null xs = []
        | otherwise = tail xs

st_c [] = []
st_c (_:xs) = xs

st_tests = [st_a,st_b,st_c]
test_cases = [[],[1],[1,2],[1..5]]
st_results = [ st test_case | st <- st_tests, test_case <- test_cases ]

-- 4.8 7.
mult :: Int -> Int -> Int -> Int
-- mult x y z = x*y*z
mult = \x -> \y -> \z -> x*y*z

-- 4.8 8. Luhn algorithm
luhnDouble :: Int -> Int
luhnDouble n  | n_2 > 9 = n_2 - 9
              | otherwise = n_2
                where n_2 = n*2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a0 a1 a2 a3 = (luhnDouble a0 + a1 + luhnDouble a2 + a3) `mod` 10 == 0

-------------------
-- 5.7 Exercises --
-------------------

-- 5.7 1.
squaresSum n = sum [x^2 | x <- [1..n]]

-- 5.7 2.
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- 5.7 3.
square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

-- 5.7 4.
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

-- 5.7 5.
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], 
                     y <- [1..n],
                     z <- [1..n], 
                     x^2 + y^2 == z^2]

-- 5.7 6.
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [p | p <- [1..n], sum (init (factors p)) == p]

-- 5.7 7.
-- [(x,y) | x <- [1,2], y <- [3,4]]
-- concat [[(x,y) | y <- [3,4]] | x <- [1,2]]

-- 5.7 7.
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find x (zip xs [0..])

-- 5.7 9. scalars
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x_i*y_i | (x_i, y_i) <- zip xs ys]

-- 5.7 10
let2int c | isLower c = ord c - ord 'a'
          | otherwise = ord c - ord 'A' + 26

int2let n | n < 26 = chr (ord 'a' + n)
          | n >= 26 = chr (ord 'A' + n - 26)

shift n c | isAlpha c = int2let ((let2int c + n) `mod` 52)
          | otherwise = c

encode n xs = [shift n x | x <- xs]

--------------------
-- Excersises 6.8 -- 
--------------------

-- 1 facotrical
factorial' n  | n < 0     = n
              | n == 1    = 1
              | otherwise = n * factorial'  (n - 1)

-- 2 sumdown
sumdown :: Int -> Int 
sumdown n | n == 0  = 0
          | n > 0   = n + sumdown (n - 1)
          | n < 0   = negate (sumdown (negate n))

-- 3 exp
expo 0 m = 0
expo n 0 = 1
expo n m = n * n `expo` (m-1)

-- 4 euclid
euclid :: Int -> Int -> Int
euclid n m  | n == m = n
            | n < m = euclid n (m-n)
            | m < n = euclid (n-m) m

-- 6 a
and' :: [Bool] -> Bool
and' [] = False
and' [x] = x
and' (x:xs) = x && and' xs

-- 6 b concat
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- 6 c
replicate'' :: Int -> a -> [a]
replicate'' 0 x = []
replicate'' n x = x:replicate'' (n-1) x

-- 6d

(!!!) :: [a] -> Int -> a
(!!!) (x:xs) 0 = x
(!!!) (x:xs) n = (!!!) xs (n-1)

-- 6e
elem' :: Eq a => a -> [a] -> Bool
elem' e [] = False
elem' e (x:xs) = if x == e then True else elem' e xs


--- 7
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y    = merge xs (x:y:ys)
                    | otherwise = y:merge (x:xs) ys


-- 8
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort fstHalf) (msort sndHalf) where
  (fstHalf, sndHalf) = halve xs

-- 9
sum' :: Num n => [n] -> n
sum' [x]    = x
sum' (x:xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' 0 _      = []
take' _ []     = []
take' n (x:xs) = x:take (n-1) xs

last' :: [a] -> a
last' [x]    = x
last' (x:xs) = last' xs


