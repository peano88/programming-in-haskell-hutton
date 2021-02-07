module Basic where

import Data.Char
import Data.List
import System.IO

size :: Int
size = 3

data Player = O | B | X deriving (Eq, Show, Ord)

type Grid = [[Player]]

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
    where
        ps = concat g
        os = length (filter (== O) ps)
        xs = length (filter (== X) ps)

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <-[0..size-1]  ]

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
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
    where 
        bar = [replicate ((size*4)-1) '-']

-- >>> do showRow [X,O,B]
-- [" | | ","X|O| "," | | "]
--

-- >>> do show $ foldr1 (zipWith (++))  [["a","b","c"],["d","e","f"]]
-- "[\"ad\",\"be\",\"cf\"]"
--

-- Game Logic
valid :: Grid -> Int -> Bool
valid g pos = pos < size^2 && 0 <= pos && concat g !! pos == B

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs: chop n (drop n xs)

move :: Grid -> Int -> Player -> [Grid]
move g pos p = 
    if valid g pos then [(chop size (xs ++ [p] ++ ys))] else []
        where (xs,B:ys) = splitAt pos (concat g)

-- AI

moves :: Grid -> Player -> [Grid]
moves g p
    | won g = []
    | full g = []
    | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

data Tree a = Node a [Tree a] deriving (Eq)

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p  ]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 9

minimax :: Tree Grid -> Tree (Grid,Player)
minimax (Node g [])
    | wins O g = Node (g,O) []
    | wins X g = Node (g,X) []
    | otherwise = Node (g,B) []
minimax (Node g ts)
    | turn g == O = Node (g, minimum ps) ts'
    | turn g == X = Node (g, maximum ps) ts'
        where
            ts' = map minimax ts
            ps = [p|Node (_,p) _ <- ts']

bestmoves :: Grid -> Player -> [Grid]
bestmoves g p = [g'| Node (g',p') _ <- ts, p' == best]
    where
        tree = prune depth (gametree g p)
        Node (_,best) ts = minimax tree
