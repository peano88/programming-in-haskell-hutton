import Data.Char
import Data.List
import System.IO

import Basic
import Exercises

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
tictactoe = run (empty 3) O

play' :: Grid -> Player -> ([Grid] -> IO Grid) -> Player -> IO ()
play' g p selector starter
    | wins O g = putStrLn "Player O wins!"
    | wins X g = putStrLn "Player X wins!"
    | full g = putStrLn "it's a draw!"
    | p == O = do 
        i <- getNat (prompt p)
        case move g i p of
            [] -> do 
                    putStrLn "ERROR: Invalid move"
                    play' g p selector starter
            [g'] -> play g' (next p) selector starter
    | p == X = do
        putStrLn "Player X is thinking"
        let gs = bestmoves g p starter
        move <- selector gs
        play move (next p) selector starter           

play ::Grid -> Player -> ([Grid] -> IO Grid) -> Player -> IO ()
play g p selector starter = do 
    cls
    goto (1,1)
    putGrid g
    play' g p selector starter


headSelector :: [Grid] -> IO Grid
headSelector = return . head

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    size <- sizeSelection
    p <- turnSelection 
    play (empty size) p headSelector p
    --play empty O randomSelector