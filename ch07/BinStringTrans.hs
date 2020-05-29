module BinStringTrans where

import Data.Char

type Bit = Int

bin2Int :: [Bit] -> Int
bin2Int bits = 
    sum [w*b | (w,b) <- zip weights bits]
    where weights = iterate (*2) 1 

bin2Int' :: [Bit] -> Int
bin2Int' = foldr (\x y -> x + 2*y) 0

int2Bin :: Int -> [Bit]
int2Bin 0 = []
int2Bin n = (mod n 2) : int2Bin (div n 2) 

make9 :: [Bit] -> [Bit]
make9 bits = parBit bits : 
             take 8 (bits ++ repeat 0) 

parBit :: [Bit] -> Int
parBit bits 
  | oddOnes bits = 1
  | otherwise = 0

oddOnes :: [Bit] -> Bool
oddOnes = odd . length . filter (==1)

encode :: String -> [Bit]
encode = concat . map (make9 . int2Bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

decode :: [Bit] -> String
decode = map (chr . bin2Int . validByte) . chop9 

validByte :: [Bit] -> [Bit]
validByte (b:bs)
  | parBit bs == b = bs
  | otherwise = error "Transmission failure"

faultyChannel :: [Bit] -> [Bit]
faultyChannel = tail 

channel :: [Bit] -> [Bit]
channel = id

transmit :: ([Bit] -> [Bit]) -> 
            String -> String
transmit chan =  decode . chan . encode

