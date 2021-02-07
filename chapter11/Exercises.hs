module Exercises where

import Basic
import System.Random (randomRIO) -- launch ghci with stack ghci --package random

treeleaves :: Tree a -> Int
treeleaves (Node _ []) = 1
treeleaves (Node _ ts) = sum . map treeleaves $ ts

countNodes :: Tree a -> Int
countNodes (Node _ ts) = (1+) . sum . map countNodes $ ts

-- >>> do putStr $ show (treeleaves (Node [Node [], Node []]))

countGameCombinations :: Int
countGameCombinations = treeleaves (gametree (empty 3)  O)

countGameNodes :: Int
countGameNodes = countNodes (gametree (empty 3) O)

randomSelector :: [Grid] -> IO Grid
randomSelector g = do
     n <- randomRIO(0, length g - 1)
     return $ g !! n

turnSelection :: IO Player
turnSelection = do
     n <- getNat "Do you want to play as first (1) or as second (2)?"
     if n > 2 then do
          putStrLn "Error: please select either 1 or 2"
          turnSelection
     else
          let selection = if n == 1 then O else X
          in return selection

sizeSelection :: IO Int
sizeSelection = do
     n <- getNat "Size of the edge of the board? ( <= 6)"
     if n > 6 then do
          putStrLn "Error: maximum sisze allowed is 6"
          sizeSelection
     else
          return n                 