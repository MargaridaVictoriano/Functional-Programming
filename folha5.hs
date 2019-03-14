import Data.Char
import Data.List
--ex40
--test if a given password is strong
forte :: String -> Bool
forte xs = length xs >= 8 && length[c | c <- xs, isLower c] > 0 && length [c | c <- xs, isUpper c] > 0 && length [c | c <- xs, isDigit c] > 0

--ex41
nub' :: (Eq a) => [a] -> [a]
nub' [] = []
nub' (x:xs) = x : nub' (filter (/=x) xs)

--ex42
intersperse1 :: a -> [a] -> [a]
intersperse1 _ []  = []
intersperse1 _ [x] = [x]
intersperse1 sep (x:xs) = x : sep : intersperse1 sep xs

--ex43

insert1 :: Ord a => a -> [a] -> [a]
insert1 x []     = [x]
insert1 x (y:ys) | x <= y = (x:y:ys)
                 | otherwise = y: (insert1 x ys)

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert1 x (isort xs)

--ex44

minim :: Ord a => [a] -> a
minim [x] = x
minim (x:xs) = min x (minim xs)

del :: Eq a => a -> [a] -> [a]
del _ [] = []
del y (x:xs) | y == x = xs
             | otherwise = x : del y xs

ssort :: Ord a => [a] -> [a]
ssort [] = []
ssort l  = m : ll
      where
	m = minimum l {-head-}
	ll = ssort (delete m l) {-tail-}

--ex45

merge1 :: Ord a => [a] -> [a] -> [a]
merge1 xs [] = xs
merge1 [] ys = ys
merge1 (x:xs) (y:ys) | x <= y = x:merge1 xs (y:ys)
      	     	       | y  < x = y:merge1 (x:xs) ys

metades :: [a] -> ([a],[a])
metades [] = ([],[])
metades l = (x,y)
  where
    n = div (length l) 2
    x = take n l
    y = drop n l


msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort l = merge1 (msort x) (msort y)
      	where
		        (x,y) = metades l

--ex46

bits :: Int -> [[Bool]]
bits 0 = [[]]
bits n = [x:l | x <- [False, True], l <- bits (n-1)]

--ex47

distrib :: a -> [a] -> [[a]]
distrib x [] = [[x]]
distrib x (y:ys) = (x:y:ys):(map(y:) (distrib x ys))

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = [ps | p <- permutations xs, ps <- distrib x p]
