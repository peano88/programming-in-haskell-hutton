{-
[’a’,’b’,’c’] :: [Char]
(’a’,’b’,’c’) :: (Char, Char,Char)
[(False,’O’),(True,’1’)] :: [(Bool, Char)]
([False,True],[’0’,’1’]) :: ([Bool], [Char])
[tail, init, reverse] :: [[a] -> [a]]    
-}

bools :: [Bool]
bools = [True, False]

nums :: [[Int]]
nums = [[1..10]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a,a)
copy x = (x,x)

apply :: (a -> b) -> a -> b
apply f = f

{-
second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x*2

palindrome :: [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a 
twice f x = f (f x)
-}