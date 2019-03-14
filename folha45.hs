--ex30
      --a
factorial :: Integer -> Integer
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n-1)
    --b
rangeProduct :: Int -> Int -> Int
rangeProduct n m | n > m = n * (rangeProduct (n-1)  m)
                 | otherwise = m
    --c
fact :: Int -> Int
fact n = rangeProduct n 1

--ex31
multr :: Int -> Int -> Int
multr 0 m = 0
multr n 0 = 0
multr n m =  n + multr n (m-1)
      -- OU
mult x y | y == 0 = 0
         | otherwise = x + mult x (y-1)

--ex32
      --usando listas em compreensao
sqrr :: Int -> Int
sqrr 0 = 0
sqrr n = last [x | x <- [1..n], x^2 <= n]

      --usando recursao
sqrr' :: Int -> Int
sqrr' n = sq n 1
sq n x | x*x == n = x
       |otherwise = sq n(x+1)

--ex33

f :: Integer -> Integer
f x = x-1
maxFun :: (Integer -> Integer) -> Integer -> Integer
maxFun f 0 = f(0)
maxFun f n = max (maxFun f (n-1)) (f n)

    --usando listas
maxl :: Ord a => [a] -> a
maxl xs = maximum xs

--ex34

anyZero :: (Integer -> Integer) -> Integer -> Bool
anyZero f 0 = f 0 == 0
anyZero f n = f n == 0 || anyZero f (n-1)

--ex35

f1 :: Integer -> Integer
f1 x = x
sumFun :: (Integer -> Integer) -> Integer -> Integer
sumFun f1 0 = 0
sumFun f 1 = f 1
sumFun f1 n = (sumFun f1 (n-1)) + (f1 n)

--ex36

mdc :: Integral a => a -> a -> a
mdc a b | b == 0 = abs a
        | otherwise = mdc b (a `mod` b)

--ex37

expn :: Integer -> Integer
expn n | n == 0 = 1
       | otherwise = 2 * (expn (n-1))

--ex38
    --test if all values are True

myAnd :: [Bool] -> Bool
myAnd [] = True --elemento neutro do and
myAnd (x:xs) = x && myAnd xs

    --test if any value is True

myOr :: [Bool] -> Bool
myOr [] =  False -- elemento neutro do or
myOr (x:xs) = x || myOr xs

    --concatenate a list of lists

myConcat :: [[a]] -> [a]
myConcat [[]] = []
myConcat ([]:ys) = myConcat ys
myConcat ((x:xs):ys) = x:myConcat(xs:ys)

    --create a list of n equal elements
myReplicate :: Int -> a -> [a]
myReplicate 0 y = []
myReplicate x y = y : myReplicate(x-1) y

    --select the n element of a list

nelem :: Int -> [a] -> a
nelem (x:xs) y | y <= 0 = x
               | otherwise = nelem (y-1) xs
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
