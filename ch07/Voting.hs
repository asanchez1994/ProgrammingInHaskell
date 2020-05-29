module Voting where

import Data.List

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count _ [] = 0 
count s (v:vs)
  | s == v = 1 + count s vs
  | otherwise = count s vs

count' :: Eq a => a -> [a] -> Int
count' x = length . filter (==x)

rmDups :: Eq a => [a] -> [a]
rmDups [] = []
rmDups (x:xs) = x : filter (/=x) (rmDups xs)

result :: Ord a => [a] -> [(Int,a)]
result vs = sort [(count v vs, v) | 
                   v <- rmDups vs]
winner :: Ord a => [a] -> a
winner = snd . last . result

ballots :: [[String]]
ballots = [["Red", "Green"],
           ["Blue"],
           ["Green","Red","Blue"],
           ["Blue","Green","Red"],
           ["Green"]]

rmEmpty :: Eq a => [[a]] -> [[a]]
rmEmpty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x)) 

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmEmpty bs) of
                [c] -> c
                (c:cd) -> winner' (elim c bs)
