-- 1

data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ( int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

instance Show Nat where
  show Zero = "0"
  show (Succ n) = "1+" ++ show n


mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
-- mult (Succ Zero) n = n
mult (Succ m) n = add n (mult m n) 

-- 2 Occurs using compare
data Tree a = Leaf a | Node (Tree a) a (Tree a)
occurs :: Ord a => a -> Tree a -> Bool
occurs a (Leaf b) = a == b
occurs a (Node l n r) = case compare a n of
                          LT -> occurs a l
                          EQ -> True
                          GT -> occurs a r


-- 3 - balanced treee checker
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a) deriving (Show)
balanced :: Tree' a -> Bool
balanced (Leaf' _)     = True
balanced (Node' l r) = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

leaves :: Tree' a -> Int
leaves (Leaf' _) = 1
leaves (Node' m n) = leaves m + leaves n

-- 4 --
balance :: [a] -> Tree' a
balance [x] = Leaf' x
balance xs = Node' (balance l) (balance r) where
  (l,r) = splitAt (length xs `div` 2) xs


-- instance Show a => Show (Tree' a) where
--   show (Leaf' t)    = "(" ++ show t ++ ")\n"
--   show (Node' l r ) = show (1, Node' r l)
--   show ((Depth d), (Node' l r )) = ""


-- 5 --
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val v) = f v
folde f g (Add l r) = g (folde f g l) (folde f g r)

-- 6 --
sizeExpr :: Expr -> Int
sizeExpr expr = folde (\_ -> 1) (+) expr

eval :: Expr -> Int
eval = folde id (+)

-- 7 --
-- ?????


