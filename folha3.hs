--ex22

sumsq :: Int
sumsq = sum [x*x| x <- [1..100]]

--ex23
      --a

aprox :: Int -> Double
aprox n = 1 + sum [-1^(x)/fromIntegral((2*x)+1) | x<-[1..n]]

      --b
aprox' :: Int -> Double
aprox' n = 1 + sum [-1^(x)/fromIntegral((x+1)^2) | x <- [1..n]]

--ex24 

divprop :: Int -> [Int]
divprop n = [ x | x <- [1..n-1], mod n x == 0]

--ex 25

perfeitos :: Int -> [Int]
perfeitos n = [ x | x <- [1..n], (sum (divprop x)) == x]

--ex26

primo :: Int -> Bool
primo x | (length (divprop x)) > 1 || x < 2 = False
        |otherwise = True

--ex 27
pascal :: Int -> [[Int]]
binom n k = div num den
         where
           num = product [1..n]
           den = (product [1..k]) * (product [1..(n-k)])
pascal n = [[binom x y] | x <- [0..n], y <- [0..x]]

--ex28

dotprod :: [Float] -> [Float] -> Float
dotprod xs ys = sum [a*b |(a,b)<-(zip xs ys)]

--ex 29

pitagoricos :: Int -> [(Int, Int, Int)]
pitagoricos n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z ^2]
