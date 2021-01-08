{-
double x = x + x

double (double 2) = (double 2) + (double 2) = (2 + 2) + (2 + 2)
double (double 2) = double (2 + 2) = (2 + 2) + (2 + 2)
double (double 2) = double (2 + 2) = double 4 = 4 + 4
-}

{-
sum [x] = sum (x:[]) = x + sum [] = x + 0 = x
-}

product :: Num a => [a] -> a
product [] = 1
product (x : xs) = x * product xs

{-
product [2,3,4] =
    product (2:[3,4]) = 2 * product[3,4] =
        2 * product (3:[4]) = 2 * 3 * product [4] =
            2 * 3 * product (4:[]) = 2 * 3 * 4 * product [] =
                2 * 3 * 4 * 1 = 24
-}

qsortReversed :: Ord a => [a] -> [a]
qsortReversed [] = []
qsortReversed (x : xs) = qsortReversed larger ++ [x] ++ qsortReversed smaller
  where
    larger = [a | a <- xs, a > x]
    smaller = [b | b <- xs, b <= x]

{-
if smaller xs = [b | b <- xs, b < x], then qsort [2,2,3,1,1] = [1,2,3]
-}