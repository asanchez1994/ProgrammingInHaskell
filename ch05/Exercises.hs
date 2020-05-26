module Exercises5 where 

sumSquares :: Int
sumSquares = sum [x^2 | x <- [1..100]]

grid :: Int -> Int -> [(Int,Int)]
grid x y = [(a,b) | a <- [0..x], b <- [0..y]]

square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

replicate' :: Int -> a -> [a]
replicate' x a = [a | _ <- [1..x]] 

pyths :: Int -> [(Int,Int,Int)]
pyths x = [(a,b,c) | a <- [1..x],
                     b <- [1..x],
                     c <- [1..x],
                     a^2 + b^2 == c^2]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n],
             sum (factors x) == 2 * x]

example :: [(Int,Int)]
example = [(x,y) | x <- [1,2], y <- [3,4]]
example2 = [(1,x) | x <- [3,4]] ++ 
           [(2,y) | y <- [3,4]] 


positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find x keyVal
  where keyVal = zip xs [0..]
        find k t = [v | (k',v) <- t,
                    k == k']

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * (ys !! i) | 
                     (x, i) <- zip xs [0..]] 
-- using zipWith
scalarProduct' :: [Int] -> [Int] -> Int
scalarProduct' xs ys = sum $ zipWith (*) xs ys 




