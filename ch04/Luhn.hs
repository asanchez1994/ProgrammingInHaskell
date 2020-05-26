module Luhn where

luhnDouble :: Int -> Int
luhnDouble x
  | double > 9 = double - 9
  | otherwise  = double
  where double = x * 2 

luhn :: Int -> Int -> 
        Int -> Int -> Bool 
luhn x y z a 
  | total `mod` 10 == 0 = True
  | otherwise           = False
  where total = luhnDouble x + y +
                luhnDouble z + a
