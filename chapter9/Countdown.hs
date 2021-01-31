module Countdown where

data Op = Add | Sub | Mul | Div | Exp

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "^"

valid :: Op -> Int -> Int -> Bool
--valid Add _ _ = True
--valid Sub x y = x > y
--valid Mul _ _ = True
--valid Div x y = x `mod` y == 0
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0
valid Exp x y = x /= 1 && y /= 1

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

-- int overflow --> <0
protectOverflow :: Int -> [Int]
protectOverflow n
 | n <= 0 = []
 | otherwise = [n]

ops :: [Op]
ops = [Add, Sub, Mul, Div, Exp]

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val x) = show x
  show (App o x y) = brak x ++ show o ++ brak y
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val x) = [x]
value (App _ x y) = values x ++ values y

eval :: Expr -> [Int]
eval (Val x) = [x | x > 0]
eval (App op l r) = [a | x <- eval l, y <- eval r, valid op x y, a <- protectOverflow (apply op x y)]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys): map (y:)  (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs):[(x:ls,rs)| (ls, rs) <- split xs]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns
                ,l <- exprs ls
                ,r <- exprs rs
                ,e <- combine l r]

-- brute force solution
solutions :: [Int] -> Int -> [Expr]
solutions ns n =[e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

-- Refining

type Result = (Expr, Int)

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, a) | o <- ops, valid o x y, a <- protectOverflow (apply o x y)] 

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n)| n > 0] -- instead of using maybe
results ns = [res | (ls,rs) <- split ns
                    ,lx <- results ls
                    ,ry <- results rs
                    ,res <- combine' lx ry]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n =[e | ns' <- choices ns, (e,m) <- results ns', m == n]
