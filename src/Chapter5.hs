module Chapter5 where
import Data.Char

ex1 :: Int
ex1 = sum [x ^ 2 | x <- [1 .. 100]]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [0 .. n-1]]

phyts :: Int -> [(Int, Int, Int)]
phyts n = [(x, y, z)| x <-[1..n], y <-[1..n], z <-[max x y..n], x^2 + y^2 == z^2]

factors :: Int -> [Int]
factors n = [x| x <-[1..(div n 2)], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <-[1..n], sum (factors x) == x] 

-- [(x,y) | x <- [1,2], y <- [3,4]]  --> (1,3), (1,4), (2,3), (2,4) = concat [[(1,3),(1,4)], [(2,3),(2,4)]]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
--positions x xs = [i | (x’,i) <- zip xs [0..], x == x’]
positions x xs = find x (zip xs [0..])

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

-- Caesar Cypher
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [(o - e)^2/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

count :: Char -> String -> Int
count c s = sum [1 | c' <- s, c == c']

lowers :: String -> Int
lowers s = sum [1 | c <- s, c >= 'a', c <= 'z']

letters :: String -> Int
letters s = sum [1 | c <- s, isLower c || isUpper c]

freqs :: String -> [Float]
freqs s = [min + maj | (min, maj) <- zip freqsm freqsM]
    where
        freqsm = [fromIntegral (count c s) / fromIntegral (letters s) | c <- ['a'..'z']]
        freqsM = [fromIntegral (count c s) / fromIntegral (letters s) | c <- ['A'..'Z']]

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

let2int :: Char -> Int
--let2int c = ord c - ord 'a'
let2int c = ord c - ord 'A'

int2let :: Int -> Char
--int2let n = chr (ord 'a' + n)
int2let n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c 
    | isLower c = int2let (let2int 'a' + (let2int c + n - let2int 'a')  `mod` 26)
    | isUpper c = int2let ((let2int c + n) `mod` 26)
    | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]


crack :: String -> String
crack xs = encode (-factor) xs
    where
        factor = head (positions (minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs
