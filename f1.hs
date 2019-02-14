--ex1
inc x = x + 1
quadrado x = x*x
dobro x = x + x
media x y = (x+y)/2
--ex2
sm a b c = (a + b <= c) && (a + c <= b) && (b + c <= a)
--ex3
her a b c = sqrt(s*(s -a)*(s-b)*(s-c))
            where s = (a + b + c)/2
--ex4
metades xs = (take half xs, drop half xs)
              where half = length(xs)`div`2
--ex5
      --a
last1 xs = xs !! i
            where i = length(xs) - 1
      --b
init1 xs = take (length(xs) - 1) xs
--ex6
      --a
binom n k = product [1..n]/product [1..k] * product [1..dif]
            where dif = n - k
--ex7
      --a
max3 x y z  | x >= y && x >= z = x
            | y >= x && y>=z = y
            | otherwise = z

min3 x y z  | x <= y && x <=z = x
            | y <= x && y <= z = y
            | otherwise = z
    --b
max3_ :: Int -> Int -> Int -> Int
max3_ x y z = max (max x y) z

min3_ :: Int -> Int -> Int -> Int
min3_ x y z = min (min x y) z

--ex8
  --a
maxOccurs :: Integer -> Integer -> (Integer, Integer)
maxOccurs a b = (max a b, n)
               where n = if a == b then 2 else 1
  --b
orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (x,y,z) = (a,b,c)
                      where a = min (min x y) z
                            b = if (a == x || a == z) && (c == x || c == z) then y else
                                  if (a == y || a == z) && (c == y || c == z) then x else z
                            c = max (max x y) z
classifica :: Int -> String
classifica x
            |x < 10 = "reprovado"
            |x < 13 = "suficiente"
            |x < 16 = "bom"
            |x < 19 = "muito bom"
            |x < 21 = "muito bom com distincao"
            |otherwise = "valor invalido, tente de novo"
classifica1 x = if x < 10 then "reprovado" else
                  if x < 13 then "suficiente" else
                    if x < 16 then "bom" else
                      if x < 19 then "muito bom" else
                        if x < 21 then "muito bom com distincao" else "valor invalido"
      
