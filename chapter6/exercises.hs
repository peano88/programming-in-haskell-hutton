sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

exp' :: Int -> Int -> Int
n `exp'` m = n * (n `exp'` (m - 1))

euclid :: Int -> Int -> Int
euclid n m
  | n == m = n
  | n < m = euclid n (m - n)
  | otherwise = euclid m (n - m)

and' :: [Bool] -> Bool
and' [] = True
and' (b : bs) = b && and' bs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x : xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n -1) x

indx :: [a] -> Int -> a
(x : _) `indx` 0 = x
(x : xs) `indx` n = xs `indx` (n - 1)

elem' :: Eq a => [a] -> a -> Bool
elem' [] _ = False
elem' (x : xs) y = if x == y then True else elem' xs y

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge xs@(x' : xs') ys@(y' : ys') = if x' <= y' then x' : merge xs' ys else y' : merge xs ys'

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs = let halfLength = length xs `div` 2 in (take halfLength xs, drop halfLength xs)

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = let halves = halve xs in merge (msort . fst $ halves) (msort . snd $ halves)

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x : xs) = x + sum xs

myTake :: [a] -> Int -> [a]
myTake [] _ = []
myTake xs 0 = []
myTake (x : xs) n = x : myTake xs (n -1)

myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs 