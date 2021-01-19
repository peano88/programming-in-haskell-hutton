module Examples 
where


import Data.Char
import Data.List (sort)

-- Transmitting

type Bit = Int

bin2int' :: [Bit] -> Int
bin2int' bits = sum [w*b | (w,b) <- zip weight bits]
    where weight = iterate (*2) 1

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

dummychannel :: [Bit] -> [Bit]
dummychannel = id

transmit :: ([Bit] -> [Bit]) -> String -> String
transmit channel = decode . channel . encode

-- First past the post
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x: rmdups (filter (/= x) xs) --more efficient : x: filter (/= x) (rmdups xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v)| v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

-- Alternative vote
rmempty :: [[a]] -> [[a]]
rmempty = filter (not . null)

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of 
    [c] -> c
    (c:cs) -> winner' (elim c bs)  

