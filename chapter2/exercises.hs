{-
    2^3*4 = (2^3) * 4
    2*3+4*5 = (2 * 3) + (4 * 5)
    2+3*4^5 = 2 + (3 * (4^5))
-}

n = a `div` length xs
  where
    a = 10
    xs = [1, 2, 3, 4, 5]

last' = head . reverse

last'' xs = xs !! (length xs - 1) -- no error handled in case of xs == []

init' xs = take (length xs - 1) xs

init'' [x] = []
init'' (x : xs) = x : init'' xs