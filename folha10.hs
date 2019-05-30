data Arv a = Vazia | No a (Arv a) (Arv a)


--ex71
sumArv :: Num a => Arv a -> a
sumArv Vazia = 0
sumArv (No x e d)  = x + sumArv e + sumArv d
--ex72
listar :: Arv a -> [a]
listar Vazia = []
listar (No x e d) = listar d ++ [x] ++ listar e


pertence :: Ord a => a -> Arv a -> Bool
pertence x Vazia = False
pertence x (No y esq dir)
  | x==y  = True
  | x<y   = pertence x esq
  | x>y   = pertence x dir

--ex73

nivel :: Int -> Arv a -> [a]
nivel _ Vazia = []
nivel 0 (No x esq dir) = [x]
nivel n (No x esq dir) = nivel (n-1) esq ++ nivel (n-1) dir

--ex75
mapArv :: (a -> b) -> Arv a -> Arv b
mapArv _ Vazia = Vazia
mapArv f (No x esq dir) = No (f x) (mapArv f esq) (mapArv f dir)
