module VoteAlternative where

import Vote ( result )
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
