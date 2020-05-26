module Exercises06 where

higherOrd :: Integral a => (a -> a -> a) -> 
                            a -> a -> a
higherOrd f i n
  | n < 0 = error "Invalid Input"
  | n == 0 =  i
  | otherwise = f n (higherOrd f i (n-1)) 

facSafe' :: Int -> Int
facSafe' = higherOrd (*) 1 

facSafe :: Int -> Int
facSafe n
  | n < 0     = error "Invalid input"
  | n == 0    = 1
  | otherwise = n * facSafe (n-1)

sumDown :: Int -> Int
sumDown = higherOrd (+) 0

expo :: Int -> Int -> Int
expo _ 0 = 1
expo x 1 = x 
expo x y = x * expo x (y-1)

eucAlg :: Int -> Int -> Int
eucAlg x y 
  | x == y    = x
  | x > y     = eucAlg (x-y) y
  | otherwise = eucAlg x (y-x)


and' :: [Bool] -> Bool
and' []         = True
and' (False:_) = False
and' (_:xs)     = and' xs

concat' :: [[a]] -> [a]
concat' []     = [] 
concat' (x:xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate (n-1) x

select' :: Int -> [a] -> a
select' _ [] = error "Index not in range"
select' 0 (x:xs) = x
select' n (x:xs) = select' (n-1) xs

elem' :: Eq a => a -> [a] -> Bool 
elem' n [] = False
elem' n (x:xs)
  | n == x    = True
  | otherwise = elem' n xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs []         = xs
merge [] ys         = ys
merge f@(x:xs) s@(y:ys)  
  | x <= y    = x : merge xs s
  | otherwise = y : merge f ys


msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort ms) (msort ns)
  where (ms,ns) = halve xs

halve :: [a] -> ([a],[a])
halve xs = (take l xs, drop l xs)
  where l = div (length xs) 2

sum' :: Num a => [a] -> a
sum' []     = 0 
sum' (x:xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' _ []     = error "Input invalid"
take' 0 xs     = []
take' n (x:xs) = x : take' (n-1) xs 

last' :: [a] -> a
last' []     = error "Input invalid" 
last' [x]    = x
last' (x:xs) = last' xs

