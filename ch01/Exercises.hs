module Exercises1 where

double :: Num a => a -> a
double x = x + x

quadruple :: Num a => a -> a
quadruple = double . double 

product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product xs

prodFold :: Num a => [a] -> a
prodFold = foldr (*) 1

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x]

revFold :: [a] -> [a]
revFold = foldl (\acc x -> x : acc) [] 

revQsort :: Ord a => [a] -> [a]
revQsort [] = [] 
revQsort (x:xs) = revQsort greater ++ [x] 
                ++ revQsort less
  where greater = [y | y <- xs, y > x]
        less = [z | z <- xs, z <= x]
