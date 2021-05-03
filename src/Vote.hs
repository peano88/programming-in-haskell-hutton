module Vote where
import Data.List (sort)

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
