--ex23*
aprox' :: Int -> Double
aprox' n = 1 + sum [-1^(x)/fromIntegral((x+1)^2) | x <- [1..n]]

--ex58
factorial :: [Integer]
factorial = 1 : zipWith (*) factorial [1..]

--ex59

merge :: Ord a => [a] -> [a] -> [a]
merge [] x = x
merge x [] = x
merge (x:xs) (y:ys)
  | x <= y = x : (merge xs (y:ys))
  | otherwise = y : (merge (x:xs) ys)

hamming :: [Integer]
hamming = [2^i * 3^j * 5^k | i <- [0..] , j <- [0..i] , k <- [0..j]]


fibs :: [Integer]
fibs = 0 : 1 : [n+m | (n,m) <- zip fibs (tail fibs)]

--ex60
somas :: [Int] -> [Int]
somas l = 0:zipWith (+) (somas l) (l)

--ex62
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate 0 list = list
rotate n (x:xs) = rotate (n-1) (xs ++ [x])
