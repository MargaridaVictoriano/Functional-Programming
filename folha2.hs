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
    --i DÚVIDA NO TIPO
twice :: (t -> t) -> t -> t
twice f x = f(f x)
