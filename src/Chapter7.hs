module Chapter7 where
import Data.Char
import Transmitting ( Bit, bin2int, int2bin )

-- [f x | x <- xs, p x ]
myfunc :: (a -> b) -> (a -> Bool) -> [a] -> [b]
myfunc f p = map f . filter p

all' :: (a -> Bool) -> [a] -> Bool
all' p xs = length (filter p xs) == length xs

any' :: (a -> Bool) -> [a] -> Bool
any' p = null . filter p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x : xs) = if p x then x : takeWhile' p xs else []

-- TODO : use foldl/foldr

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x ys -> f x : ys) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x : xs else xs) []

dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> 10 * acc + x) 0

curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f x = f . ((,) $ x)

-- curry' f = \x y -> f (x,y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(x, y) -> f x y

unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

map'' f = unfold null (f . head) tail

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold null (take 8) (drop 8)

iterate' f x = x : unfold (\_ -> False) f f x

parity :: [Bit] -> Bit
parity bits = if odd (sum bits) then 1 else 0

make8parity :: [Bit] -> [Bit]
make8parity bits = parity bits : take 8 (bits ++ repeat 0)

encodeParity :: String -> [Bit]
encodeParity = concat . map (make8parity . int2bin . ord)

chop8parity :: [Bit] -> [[Bit]]
chop8parity [] = []
chop8parity bits =
  let (x : xs) = take 9 bits
      evens a b = (even a && even b) || (odd a && odd b)
   in if evens (x) (sum xs) then xs : chop8parity (drop 9 bits) else error "Broken Transmission"

decodeParity :: [Bit] -> String
decodeParity = map (chr . bin2int) . chop8parity

faultyChannel :: [Bit] -> [Bit]
faultyChannel = tail

transmitParity :: ([Bit] -> [Bit]) -> String -> String
transmitParity channel = decodeParity . channel . encodeParity

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f _ [x] = [f x]
altMap f g (x : y : ys) = f x : g y : altMap f g ys


luhn :: [Int] -> Bool
luhn xs =  transform xs `mod`  10 == 0
  where lDouble x = if x /= 9 then (x * 2) `mod`  9 else x
        transform = sum . altMap id lDouble . reverse
