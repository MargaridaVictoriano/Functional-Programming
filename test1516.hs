f 0 = 0
f n = n*f(n-1)

troca :: (a,b) -> (b,a)
troca (x,y) = (y,x)

g :: (Ord a, Num a) => a -> a -> a
g x y | x <= y = g x (y-1)
      | otherwise = x + y

ttriangulos :: Eq a => a -> a -> a -> String
ttriangulos a b c
                  | a == b && a == c && b == c = "Equilatero"
                  | a /= b && a /= c && b /= c = "Escaleno"
                  | otherwise = "Isosceles"

retangulo :: Int -> Int -> Int -> Bool
retangulo a b c = (a^2 == b^2+c^2) || (b^2 == a^2+c^2) || (c^2 == a^2 + b^2)

maiores :: Ord a => [a] -> [a]
maiores [] = []
maiores [x] = [x]
maiores (x:y:ys)
                |x >= y = x : maiores(y:ys)
                | otherwise = maiores(y:ys)

somapares :: [(Int,Int)] -> [Int]
somapares xs = [x+y | (x,y) <- xs ]

sumpair :: [(Int, Int)] -> [Int]
sumpair [] = []
sumpair ((x,y):xs) = (x + y) : sumpair xs

itera :: Int -> (a->a) -> a -> a
itera 0 f v = v
itera x f v | x > 0 = f (itera (x-1) f v)

mult :: Int -> Int -> Int
mult x y = itera x (+y) 0
