sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum xs

foldSum :: Num a => [a] -> a
foldSum = foldr (+) 0

