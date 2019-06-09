aprov :: [Int] -> [Char]
aprov xs = map f xs
            where f x
                    | x >= 15 = 'A'
                    | otherwise = 'R'

injust :: [Int] -> Int
injust xs = length (filter (\x -> (x >=10) && (x < 15)) xs)
injust1 xs = length [x | x <- xs, x<15, x>=10]
injust2 xs = length (filter (< 15) (filter (>= 10) xs))

repete :: a -> [[a]]
repete x = []: map(x:) (repete x)
{-
maximo :: IO ()
maximo = maximo_aux 0
maximo_aux :: Int -> IO ()
maximo_aux m = do x <- getLine
                  let n = read x in
                      if n == 0 then print m
                      else (max n m)
-}
maximo = do xs <- maximo_aux
            putStrLn(show (maximum xs))
maximo_aux :: IO [Int]
maximo_aux = do x <- getLine
                let n = read x in
                    if n == 0 then
                        return []
                    else do xs <- maximo_aux
                            return (n:xs)

compLr :: [a -> a] -> a -> a

compLr [] v = v
compLr (f:fs) v = f (compLr fs v)

compL :: [a -> a] -> a -> a

compL fs v = foldr f v fs
        where f g x = g x

data Arv a = Vazia | No a (Arv a) (Arv a)
soma :: Num a => Arv a -> a
soma Vazia = 0
soma (No x esq dir) = x + (soma esq) + (soma dir)

foldrtree :: (a -> b -> b -> b) -> b -> Arv a -> b
foldrtree f v Vazia = v
foldrtree f v (No x esq dir) = f x (foldrtree f v esq) (foldrtree f v dir)
