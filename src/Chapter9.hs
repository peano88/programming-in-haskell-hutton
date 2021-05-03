module Chapter9 where
import Countdown

choices' ns = [s | ys <- subs ns, s <- perms ys ]

removefirst :: Eq a => a -> [a] -> [a]
removefirst _ [] = []
removefirst x (n:ns)
    | x == n = ns
    | otherwise = n:removefirst x ns

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x:xs) ys = x `elem` ys && isChoice xs (removefirst x ys)
