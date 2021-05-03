module Chapter4 where
halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs
  | odd $ length xs = error "list not compatible"
  | otherwise = let m = length xs `div` 2 in (take m xs, drop m xs)

thirdWrap :: ([a] -> a) -> [a] -> a
thirdWrap f xs = if length xs > 2 then f xs else error "list not compatible"

-- with head and tail
third :: [a] -> a
third = thirdWrap (head . tail . tail)

-- with index
third' :: [a] -> a
third' = thirdWrap (!! 2)

-- pattern matching
third'' :: [a] -> a
third'' = thirdWrap withPattern
  where
    withPattern (_ : _ : x : _) = x

safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' xs
  | null xs = []
  | otherwise = tail xs

safetail'' [] = []
safetail'' xs = tail xs

{-
True || False = True
True || True = True
False || True = True
False || False = False

simplifying
False || False = False
_ || _ = True

but this is not as efficient as :
True || _ = True
_ || True = True
False || False = False
-}

{-
a && b = if a then if b then True else False else False
-}

{-
a && b = if a then b else False
-}

mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x * y * z))

luhnDouble :: Int -> Int 
luhnDouble = (`mod` 9) . (*2)

luhn :: Int -> Int -> Int -> Int -> Bool 
luhn x y z w = (luhnDouble x + y + luhnDouble z + w) `mod` 10 == 0