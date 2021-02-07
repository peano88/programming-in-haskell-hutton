import Data.Char
import Data.List
import System.IO

import Basic
import System.Random (randomRIO) -- launch ghci with stack ghci --package random

run :: Grid -> Player -> IO ()
run g p = do
    cls
    goto (1,1)
    putGrid g
    run' g p


run' :: Grid -> Player -> IO ()
run' g p
    | wins O g = putStrLn "Player O wins"
    | wins X g = putStrLn "Player X wins"
    | full g = putStrLn "It's a draw"
    | otherwise = 
        do
            i <- getNat (prompt p)
            case move g i p of
                [] -> do 
                    putStrLn "ERROR: Invalid move"
                    run' g p
                [g'] -> run g' (next p)

tictactoe :: IO ()
tictactoe = run empty O

play' :: Grid -> Player -> ([Grid] -> IO Grid) -> IO ()
play' g p selector
    | wins O g = putStrLn "Player O wins!"
    | wins X g = putStrLn "Player X wins!"
    | full g = putStrLn "it's a draw!"
    | p == O = do 
        i <- getNat (prompt p)
        case move g i p of
            [] -> do 
                    putStrLn "ERROR: Invalid move"
                    play' g p selector
            [g'] -> play g' (next p) selector
    | p == X = do
        putStrLn "Player X is thinking"
        let gs = bestmoves g p
        move <- selector gs
        (play $! move) (next p) selector            

play ::Grid -> Player -> ([Grid] -> IO Grid) -> IO ()
play g p selector = do 
    cls
    goto (1,1)
    putGrid g
    play' g p selector

randomSelector :: [Grid] -> IO Grid
randomSelector g = do
     n <- randomRIO(0, length g - 1)
     return $ g !! n

headSelector :: [Grid] -> IO Grid
headSelector = return . head

main :: IO ()
main = do 
    hSetBuffering stdout NoBuffering
    play empty O headSelector
    --play empty O randomSelector