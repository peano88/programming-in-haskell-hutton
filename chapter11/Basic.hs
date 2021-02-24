module Basic where

import Data.Char
import Data.List
import System.IO

data Player = O | B | X deriving (Eq, Show, Ord)

type Grid = [[Player]]

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Int -> Grid
empty n = replicate n (replicate n B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player -> Player
turn g starter
    | os < xs = O
    | os == xs = starter
    | otherwise = X 
    where
        ps = concat g
        os = length (filter (== O) ps)
        xs = length (filter (== X) ps)

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <-[0..(length g-1)]  ]

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
    where
        line = all (== p)
        rows = g
        cols = transpose g
        dias = [diag g, diag (map reverse g)]

won :: Grid -> Bool
won g = wins O g || wins X g

-- Displaying
cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int,Int) -> IO ()
goto (x,y) = putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

showPlayer :: Player -> [String]
showPlayer p = let pshow = case p of 
                    B -> " "
                    player -> show player in ["   ", " " ++ pshow ++ " ", "   "]

interleave :: a -> [a] -> [a]
interleave _ [] = []
interleave _ [y] = [y]
interleave x (y:ys) = y:x:interleave x ys

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
    where
        bar = replicate 3 "|"
        beside = foldr1 (zipWith (++))

getNat :: String -> IO Int
getNat s = do
    putStr s
    xs <- getLine
    if xs /= [] && all isDigit xs then
        return (read xs)
    else
        do
            putStrLn "ERROR: Invalid argument"
            getNat s

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

putGrid :: Grid -> IO ()
putGrid g = putStrLn . unlines . concat . interleave bar . map showRow $ g
    where 
        bar = [replicate (((length g)*4)-1) '-']

-- >>> do showRow [X,O,B]
-- [" | | ","X|O| "," | | "]
--

-- >>> do show $ foldr1 (zipWith (++))  [["a","b","c"],["d","e","f"]]
-- "[\"ad\",\"be\",\"cf\"]"
--

-- Game Logic
valid :: Grid -> Int -> Bool
valid g pos = pos < (length g)^2 && 0 <= pos && concat g !! pos == B

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs: chop n (drop n xs)

move :: Grid -> Int -> Player -> [Grid]
move g pos p = 
    if valid g pos then [(chop (length g) (xs ++ [p] ++ ys))] else []
        where (xs,B:ys) = splitAt pos (concat g)

-- AI

moves :: Grid -> Player -> [Grid]
moves g p
    | won g = []
    | full g = []
    | otherwise = concat [move g i p | i <- [0..(((length g)^2)-1)]]

data Tree a = Node a [Tree a] deriving (Eq)


-- Exercises
maxDepth :: Tree a -> Int
maxDepth (Node _ []) = 0 -- avoid maximum exception on empty list
maxDepth (Node _ ts) = (1+) . maximum . map maxDepth $ ts

smallerDepth :: Tree a -> Tree a -> Ordering
smallerDepth a b = compare (maxDepth a) (maxDepth b)    
--

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p  ]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Grid -> Int
depth g = if length g < 4 then 9 else 3

minimax :: Tree Grid -> Player -> Tree (Grid,Player)
minimax (Node g []) _
    | wins O g = Node (g,O) []
    | wins X g = Node (g,X) []
    | otherwise = Node (g,B) []
minimax (Node g ts) starter
    | turn g starter == O = Node (g, minimum ps) ts'
    | turn g starter == X = Node (g, maximum ps) ts'
        where
            ts' = map (flip minimax starter) ts
            ps = [p|Node (_,p) _ <- ts']

bestmoves :: Grid -> Player -> Player-> [Grid]
bestmoves g p starter = [g'| Node (g',p') _ <- ts', p' == best]
    where
        tree = prune (depth g) (gametree g p)
        Node (_,best) ts = minimax tree starter
        --ts' = sortBy smallerDepth ts
        ts' = ts