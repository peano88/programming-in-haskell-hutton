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

data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)

countLeaves :: Tree' a -> Int
countLeaves (Leaf' _) = 1
countLeaves (Node' l r) = countLeaves l + countLeaves r

balanced :: Tree' a -> Bool
balanced (Leaf' _) = True
balanced (Node' l r) = abs (countLeaves l - countLeaves r) <= 1 && balanced l && balanced r

