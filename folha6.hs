--Ordem Superior : 48, 51, 49, 50, 52, 53, 54, 56
--ex48 : Escrever [x^2+1 | x <- [1..100], x 'mod' 3 /= 0] usando map e filter e [1..100]
-- map f (filter p xs)
--ou
-- (map f . filter p) xs

--ex49

(+++) :: [a] -> [a] -> [a]
(+++) l1 l2 = foldr (:) l2 l1

myConcat :: [[a]] -> [a]
myConcat xs = foldr (++) [] (xs)

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = f x (myreverse xs)
             where
             	 f x y = y ++ [x]

myreverse' :: [a] -> [a]
myreverse' l = foldr f [] l
             where f x y = y ++ [x]

lreverse' l = foldl f [] l
            where f y x = x:y

myElem :: Eq a => a -> [a] -> Bool
myElem n (x:xs) = any (==n) xs

--ex50

dec2int :: [Int] -> Int
dec2int xs = foldr (\x l -> (l*10 + x)) (0) (reverse xs)

toInt :: [Int] -> Int
toInt = foldl addDigit 0
   where addDigit num d = 10*num + d


--ex51
myzipWith :: (a->b->c) -> [a] -> [b] -> [c]
myzipWith f xs ys = [f x y | (x,y) <- zip xs ys]

rzipWith :: (a->b->c) -> [a] -> [b] -> [c]
rzipWith _ [] _ = []
rzipWith _ _ [] = []
rzipWith f (x:xs) (y:ys) = f x y : rzipWith f xs ys

--ex53
shift :: [a] -> [a]
shift [] = []
shift (x:xs) = xs ++ [x]

rotateaux :: [a] -> [a] -> [[a]]
rotateaux l [] = [l]
rotateaux l (x:xs) = f x (rotateaux l xs)
     where
       f x y = (shift (head y)) : y

rotate :: [a] -> [[a]]
rotate l = rotateaux l (tail l)

rotate' l = foldr f [l] [1..(length l)-1]
        where f x y = (shift (head y)) : y
