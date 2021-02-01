import System.IO

putStr' :: String -> IO ()
putStr' s = sequence_[putChar c | c <- s]

-- Exercise 2 and 3 already done in nim.hs, the 3 since i did not know sequence_ i used my chainAction

aux :: (Int,Int) -> Int -> (Int,Int)
aux (tot, remaining) new = (tot+new, remaining - 1)

play :: (Int,Int) -> IO ()
play (tot, n) = do
    if n == 0 then
        do
            putStrLn $ "The total is: " ++ show tot
    else
        do
            stringn <- getLine
            let m = read stringn :: Int
            play $ aux (tot,n) m


adder :: IO ()
adder = do
    putStrLn "How many numbers"
    stringn <- getLine
    play (0, read stringn :: Int)


adder' :: IO ()
adder' = do
    putStrLn "How many numbers"
    stringn <- getLine
    let n = read stringn :: Int
    strings <- sequence $ replicate n getLine
    putStrLn $ "The total is: " ++ (show $ sum (map read  strings))

getCh :: IO Char
getCh = do 
    hSetEcho stdin False
    x <- getChar
    hSetEcho stdin True
    return x

getLineAcc :: String -> IO String
getLineAcc s = do
    x <- getCh
    case x of 
        '\n' -> return s
        '\DEL' -> if null s then
                    getLineAcc s
                  else do
            putChar '\b'
            getLineAcc (init s) 
        c -> do
            putChar c
            getLineAcc (s ++ [c])

getLine' :: IO String
getLine' = getLineAcc ""

-- >>> do putStr' "Answer: 42"
-- Answer: 42
--
