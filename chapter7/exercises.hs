-- [f x | x <- xs, p x ]
myfunc :: (a -> b) -> (a -> Bool) -> [a] -> [b]
myfunc f p = map f . filter p

all' :: (a -> Bool) -> [a] -> Bool
all' p xs = length (filter p xs ) == length xs

any' :: (a -> Bool) -> [a] -> Bool
any' p  = null . filter p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) = if p x then x: takeWhile' p xs else [] 
-- TODO : use foldl/foldr

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x ys -> f x : ys) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []