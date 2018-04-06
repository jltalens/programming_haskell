module BinaryTransmitter7 where

import Data.Char

-- In this exercise we interpret the bits in reverse order 1011 = (1* 1) + (0 * 2) + (1 * 4) + (1 * 8)
type Bit = Int

{--
bin2int :: [Bit] -> Int
bin2int bits = sum [w * b | (w,b) <- zip weigths bits]
  where weigths = iterate (*2) 1
--}

-- (1*a) + (2*b) + (4*c) + (8*d) = ... = a + 2 * (b + 2 * ( c + 2 * (d + 2 * 0)))
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

{--
13 div 2 = 6 rem 1
6 div 2 = 3 rem 0
3 div 2 = 1 rem 1
1 div 2 = 0 rem 1
--}
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- "abc" -> [make8(int2bin(ord('a'))), ...]
encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id
