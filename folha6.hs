--ex48
-- map f (filter p xs)
--ou
-- (map f . filter p) xs

--ex51
myzipWith :: (a->b->c) -> [a] -> [b] -> [c]
myzipWith f xs ys = [f x y | (x,y) <- zip xs ys]

rzipWith :: (a->b->c) -> [a] -> [b] -> [c]
rzipWith _ [] _ = []
rzipWith _ _ [] = []
rzipWith f (x:xs) (y:ys) = f x y : rzipWith f xs ys


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