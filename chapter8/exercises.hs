data Nat = Zero | Succ Nat deriving (Show)

add :: Nat -> Nat -> Nat
add Zero x = x
add (Succ a) x = Succ (add a x)

mult :: Nat -> Nat -> Nat
mult Zero x = Zero
mult (Succ a) x = add x (mult a x)

data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Eq a => Tree a -> a -> Bool
occurs (Leaf x) y = x == y
occurs (Node l x r) y = x == y || occurs l y || occurs r y

occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y) = x == y
occurs' x (Node l y r)
  | x == y = True
  | x < y = occurs' x l
  | otherwise = occurs' x r

occurs'' :: Ord a => a -> Tree a -> Bool
occurs'' x (Leaf y) = x == y
occurs'' x (Node l y r) = case compare x y of
  EQ -> True
  LT -> occurs'' x l
  GT -> occurs'' x r

data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a) deriving Show

countLeaves :: Tree' a -> Int
countLeaves (Leaf' _) = 1
countLeaves (Node' l r) = countLeaves l + countLeaves r

balanced :: Tree' a -> Bool
balanced (Leaf' _) = True
balanced (Node' l r) = abs (countLeaves l - countLeaves r) <= 1 && balanced l && balanced r

toBalanced :: [a] -> Tree' a
toBalanced [] = error "[] can't be cnverted into a balanced tree"
toBalanced [x] = Leaf' x
toBalanced xs = Node' ( toBalanced . fst $ halves xs) (toBalanced . snd $ halves xs)
    where halves xs = let n = length xs `div` 2 in (take n xs, drop n xs)
-- next time:
--toBalanced xs = Node'(toBalanced ys) (toBalanced zs) where (ys,zs) = splitAt (length xs `div` 2) xs

data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)
