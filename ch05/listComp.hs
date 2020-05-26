length' :: [a] -> Int
length' xs = sum [ 1 | _ <- xs]

concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <-xs]

firsts :: [(a,b)] -> [a]
firsts xs = [x | (x,_) <- xs]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

isPrime :: Int -> Bool
isPrime n = factors n == [1,n]

primeFactors :: Int -> [Int]
primeFactors n = [x | x <- (factors n), 
                  isPrime n]

listOfPrimes :: Int -> [Int]
listOfPrimes n = [x | x <- [2..n], 
                  isPrime x]

dict = [('a',2), ('b',4), 
        ('c',3), ('b',5)]

find :: Eq b => b -> [(b, a)] -> [a]
find k v = [value | (key,value) <- v,
            key == k] 

pairs :: [a] -> [(a,a)]
pairs ns = zip ns $ tail ns

sorted :: Ord a => [a] -> Bool
sorted ns = 0 == (length [1 | (fst, snd) <- pairs ns, fst > snd])

sorted' :: Ord a => [a] -> Bool
sorted' ns = and [x <= y | (x,y) <- pairs ns]

positions :: Eq a => a -> [a] -> [Int]
positions v xs = 
  let zipList = zip xs [0..]
  in [ind | (val,ind) <- zipList, val == v]
