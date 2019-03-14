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
