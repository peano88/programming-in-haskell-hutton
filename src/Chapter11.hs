module Chapter11 where

import           Basic
import           System.Random                  ( randomRIO ) -- launch ghci with stack ghci --package random

treeleaves :: Tree a -> Int
treeleaves (Node _ []) = 1
treeleaves (Node _ ts) = sum . map treeleaves $ ts

countNodes :: Tree a -> Int
countNodes (Node _ ts) = (1 +) . sum . map countNodes $ ts

-- >>> do putStr $ show (treeleaves (Node [Node [], Node []]))

countGameCombinations :: Int
countGameCombinations = treeleaves (gametree (empty 3) O)

countGameNodes :: Int
countGameNodes = countNodes (gametree (empty 3) O)

randomSelector :: [Grid] -> IO Grid
randomSelector g = do
  n <- randomRIO (0, length g - 1)
  return $ g !! n

turnSelection :: IO Player
turnSelection = do
  n <- getNat "Do you want to play as first (1) or as second (2)?"
  if n > 2
    then do
      putStrLn "Error: please select either 1 or 2"
      turnSelection
    else let selection = if n == 1 then O else X in return selection

sizeSelection :: IO Int
sizeSelection = do
  n <- getNat "Size of the edge of the board? ( <= 6)"
  if n > 6
    then do
      putStrLn "Error: maximum sisze allowed is 6"
      sizeSelection
    else return n

keepSubtree :: Eq a => a -> Tree a -> [Tree a]
keepSubtree y (Node x ts) | x == y    = [Node x ts]
                          | otherwise = concatMap (keepSubtree y) ts

pruneTree :: Tree a -> Tree a
pruneTree (Node x _) = Node x []

bestmoves' :: Grid -> Player -> Player -> Tree Grid -> [Grid]
bestmoves' g p starter gameTree = [ g' | Node (g', p') _ <- ts', p' == best ]
 where
  tree              = head (keepSubtree g gameTree)
  Node (_, best) ts = minimax tree starter
  --ts' = sortBy smallerDepth ts
  ts'               = ts

data AlphaBeta a = UnsetLow | Set a | UnsetHigh deriving (Eq,Ord)

evalAlpha :: Ord a => AlphaBeta a -> a -> AlphaBeta a
evalAlpha UnsetLow b = Set b
evalAlpha (Set a)  b = Set (max a b)

evalBeta :: Ord a => AlphaBeta a -> a -> AlphaBeta a
evalBeta UnsetHigh b = Set b
evalBeta (Set a)   b = Set (min a b)



algStep
  :: Ord b
  => Tree a
  -> (a -> b)
  -> Bool
  -> AlphaBeta b
  -> AlphaBeta b
  -> (Tree (a, b), AlphaBeta b, AlphaBeta b)
algStep (Node x ts) evaluate maximizing alpha beta =
  (Node (x, gain) explored, alpha', beta')
 where
  explore _ _ _ _ [] = []
  explore evaluate maximizing alpha beta (t' : ts')
    | beta <= alpha
    = []
    | otherwise
    = let (y, alpha'', beta'') = algStep t' evaluate maximizing alpha beta
      in  y : explore evaluate maximizing alpha'' beta'' ts'
  explored = explore evaluate (not maximizing) alpha beta ts
  gain
    | maximizing = if null explored
      then evaluate x
      else maximum [ p | (Node (_, p) _) <- explored ]
    | otherwise = if null explored
      then evaluate x
      else minimum [ p | (Node (_, p) _) <- explored ]
  alpha' = if maximizing then evalAlpha alpha gain else alpha
  beta'  = if not maximizing then evalBeta beta gain else beta

alphabeta :: Ord b => Tree a -> (a -> b) -> Bool -> Tree (a, b)
alphabeta x evaluate maximizing =
  let (y, _, _) = algStep x evaluate maximizing UnsetLow UnsetHigh in y

showT (Node x ts) = "Node" ++ show x ++ "[" ++ concatMap showT ts ++ "]"

-- >>> do putStr $ showTree (alphabeta (Node 1 [Node 3 [Node 7 [], Node 5 []], Node 3 [Node 8 [], Node 5 []]])) id False 

-- >>> do putStr $ showTree (alphabeta (Node 1 [Node 3 [Node 7 [], Node 5 []], Node 3 [Node 8 [], Node 5 []]])) id True
