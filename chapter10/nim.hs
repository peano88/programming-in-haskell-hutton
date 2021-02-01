import Data.Char

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid bs row num = bs !! (row -1) >= num

move :: Board -> Int -> Int -> Board
move bs row num = [update r n | (r, n) <- zip [1 ..] bs]
  where
    update r n = if row == r then n - num else n

putRow :: Int -> Int -> IO ()
putRow row num = putStrLn r
  where
    r = show row ++ " " ++ concat (replicate num "* ")

chainAction :: [IO ()] -> IO ()
chainAction [] = return ()
chainAction (a : as) = do
  a
  chainAction as

putBoard :: Board -> IO ()
putBoard = chainAction . map (\(r, n) -> putRow r n) . zip [1 ..]

getDigit :: String -> IO Int
getDigit prompt = do
  putStr prompt
  x <- getChar
  if isDigit x
    then return $ digitToInt x
    else do
      putStrLn "Error: Invalid Digit"
      getDigit prompt

newline :: IO ()
newline = putChar '\n'

play :: Board -> Int -> IO ()
play board player = do
  newline
  putBoard board
  if finished board
    then do
      newline
      putStrLn $ "Player " ++ show player ++ " wins!"
    else do
      newline
      putStrLn $ "Player " ++ show player
      row <- getDigit "Enter a row number: "
      number <- getDigit "Enter stars to remove"
      if valid board row number
        then play (move board row number) (next player)
        else do
          newline
          putStrLn "Error: Invalid move"
          play board player

nim :: IO ()
nim = play initial 1