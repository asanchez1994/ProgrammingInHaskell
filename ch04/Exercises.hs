module Exercises4 where

halve :: [a] -> ([a],[a])
halve xs = (take half xs, drop half xs)
  where half = length xs `div` 2

thirdHead :: [a] -> a
thirdHead = head . tail . tail

thirdIndex :: [a] -> a
thirdIndex xs = xs !! 2

thirdMatch :: [a] -> a
thirdMatch (x:y:z:xs) = z

safeTailCond :: [a] -> [a]
safeTailCond xs =
  if null xs == True
  then []
  else tail xs

safeTailGuard :: [a] -> [a]
safeTailGuard xs
  | null xs   = []
  | otherwise = tail xs

safeTailMatch :: [a] -> [a]
safeTailMatch []     = []
safeTailMatch (x:xs) = xs 

or' :: Bool -> Bool -> Bool
True `or'` _    = True 
_    `or'` True = True
_    `or'` _    = False

myAnd :: Bool -> Bool -> Bool
myAnd a b = 
  if a == True
  then 
    if b == True 
    then True
    else False
  else False

myAndShort :: Bool -> Bool -> Bool
myAndShort a b =
  if a == True
  then b
  else False

multCurry :: Int -> Int -> Int -> Int
multCurry = \a -> (\b -> (\c -> a*b*c))


