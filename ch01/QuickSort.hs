module Quicksort where

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort less ++ [x] 
                   ++ quickSort greater
    where less = [y | y <- xs, y <= x]
          greater = [z | z <- xs, z > x] 

