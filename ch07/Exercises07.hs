module Exercises07 where

import Luhn 

reWrite :: (a -> Bool) -> 
           (a -> b) -> 
           [a] -> [b]
reWrite b f xs = map f (filter b xs)

all' f = and . map f
any' f = or . map f

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) 
  | f x       = x : takeWhile' f xs
  | otherwise = [] 

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f t@(x:xs) 
  | f x       = dropWhile' f xs
  | otherwise = t

mapFold :: (a -> b) -> [a] -> [b]
mapFold f = foldr (\x acc -> f x : acc) [] 

filterFold :: (a -> Bool) -> [a] -> [a]
filterFold f = foldr 
  (\x acc -> if f x then x:acc else acc) []

dec2Int :: [Int] -> Int
dec2Int = foldl (\acc x -> acc*10 + x) 0

curry' :: ((a,b) -> c) -> a -> b -> c
curry' f = \y -> \x -> f (y,x)

uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f (x,y) = f x y 


unfold :: (a -> Bool) -> (a -> b) -> 
          (a -> a) -> a -> [b]
unfold p h t x 
  | p x  = []
  | otherwise = h x : unfold p h t (t x)

int2Bin :: Integer -> [Integer]
int2Bin = unfold (==0) (`mod` 2) (`div` 2)

chop8 :: [Int] -> [[Int]]
chop8 = unfold null (take 8) (drop 8)

mapUnfold :: (a -> b) -> [a] -> [b]
mapUnfold f = unfold null (f . head) tail

iterUnfold :: (a -> a) -> a -> [a]
iterUnfold = unfold (const False) id  

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:[])   = f x : []
altMap f g (x:y:xs) = f x : g y : altMap f g xs

luhnImp :: [Int] -> Bool
luhnImp numb
  | tot `mod` 10 == 0 = True 
  | otherwise         = False
  where tot = sum $ altMap luhnDouble id numb

