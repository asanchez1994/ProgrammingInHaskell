module RecurPractice where

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)

product' :: Num a => [a] -> a
product' []     = 1
product' (x:xs) = x * product xs 

length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length xs

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse xs ++ [x]

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x t@(y:ys) 
  | x <= y    = x : t
  | otherwise = y : insert x ys  

-- insertion sort
isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip xs ys

drop' :: Int -> [a] -> [a]
drop' _ []     = []
drop' 0 xs     = xs
drop' n (x:xs) = drop (n-1) xs 

map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

-- multiple recursion
fib ::  Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

-- mutual recursion
even' :: Int -> Bool
even' 0 = True 
even' n = odd (n-1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even (n-1)

evens :: [a] -> [a]
evens []     = []
evens (x:xs) = x : odds xs 

odds :: [a] -> [a]
odds []     = []
odds (x:xs) = evens xs


init' :: [a] -> [a]
init' [_] = []
init' (x:xs) = x : init xs
