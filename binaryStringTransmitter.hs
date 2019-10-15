import Data.Char

type Bit = Int

binToInt :: [Bit] -> Int
{-
  binToInt bits = sum [w*b | (w,b) <- zip weights bits]
    where weights = iterate (*2) 1
-}
binToInt = foldr (\x y -> x + 2*y) 0

intToBin :: Int -> [Bit]
intToBin 0 = []
intToBin integer = integer `mod` 2 : intToBin half
  where half = integer `div` 2

make8 :: [Bit] -> [Bit]
make8 binary = take 8 (binary ++ repeat 0)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 rest
  where rest = drop 8 bits

encode :: String  -> [Bit]
encode = concat . map (make8 . intToBin . ord)

decode :: [Bit] -> String
decode = map (chr . binToInt) . chop8

transmit :: String -> String
transmit = decode . channel . encode 
  where channel = id