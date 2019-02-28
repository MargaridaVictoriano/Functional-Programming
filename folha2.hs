-- 15
--  1) f :: (Int,Int) -> Int
--  2) f :: a -> Int ; g :: Int -> a
--  3) f :: a -> Int -> Int ; g :: a
--  4) f :: a -> [Int] -> [Int] ; g :: a
--  5) f :: (Int,Int) -> [Int -> Int]

--ex 16
{-
  head :: (f g) 5 :: [Int]
  f :: a -> [t]
  g :: a
  head :: [t] -> t
  t = t1 -> t2
  t1 :: Int
  t2 :: [Int]
-}

-- f :: a -> Int -> [[Int]]
-- g :: a

--ex17
      --a
segundo :: [a] -> a
segundo xs = head(tail xs)
      --b
trocar :: (a,b) -> (b,a)
trocar (x,y) = (y,x)
      --c
par :: a  -> b -> (a,b)
par x y = (x,y)
      --d
dobro :: Num a => a -> a
dobro x = 2*x
    --e
metade :: Fractional a => a -> a
metade x = x/2
    --f
minuscula :: Char -> Bool
minuscula x = x >= 'a' && x <= 'z'
    --g
intervalo :: Ord a => a -> a -> a -> Bool
intervalo x a b = x >= a && x <= b
    --h
palindromo :: Eq a => [a] -> Bool
palindromo xs = reverse xs == xs
    --i
twice :: (t -> t) -> t -> t
twice f x = f(f x)

-- 18
--  inc x = x + 1:
--     Admissível -> inc :: Int -> Int; inc :: Float -> Float
--     Geral -> inc :: Num a => a -> a
--  dobro x = x*2:
--     Amissível -> dobro :: Integer -> Integer; dobro :: double -> double
--     Geral -> Num a => a -> a
--  quadrado x = x*x:
--     Admissível -> quadrado :: Int -> Int
--     Geral -> quadrado :: Num a => a -> a
--  media x y = (x+y)/2:
--     Admissível -> media :: Int -> Int -> Float
--     Geral -> media :: Fractional a => a -> a -> a

-- 19


--  2)
e2 :: Char -> Bool -> Bool
e2 x b | x == 'a' = b
        | otherwise = b&&b


--  4)
e4 :: Eq a => a -> [a] -> Bool
e4 a xs = a == head xs


--  5)
e5 :: Eq a => a -> [a] -> [a]
e5 a xs | length(xs) == 0 = []
          | otherwise = a:xs

--  6)
e6 :: Ord a => a -> a -> a
e6 a b = if a>b then a else b
