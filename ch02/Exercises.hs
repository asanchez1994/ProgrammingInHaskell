module Exercises2 where

n = a `div` length xs
    where a = 10
          xs = [1,2,3,4,5] 

last' :: [a] -> a
last' = head . reverse

lastRecur :: Eq a => [a] -> Maybe a
lastRecur [] = Nothing
lastRecur (x:xs)
  | xs == []  = Just x
  | otherwise = lastRecur xs

init' :: [a] -> [a]
init' [] = error "cannot take init of empty list"
init' xs = take (length xs - 2) xs

initRecur :: Eq a => [a] -> [a]
initRecur []     = error "cannot take initRecur of empty list"
initRecur (x:[]) = []
initRecur (x:xs) = x : initRecur xs
