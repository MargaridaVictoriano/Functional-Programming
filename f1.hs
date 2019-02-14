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

--ex10
xor :: Bool -> Bool -> Bool
xor a b | a == False && b == False = False
        | a == False && b == True = True
        | a == True  && b == False = True
        | a == True  && b == True = False
xor1 :: Bool -> Bool -> Bool
xor1 False False = False
xor1 True False = True
xor1 False True = False
xor1 True True = True


--ex11
safetail :: [a] -> [a]
safetail [] = []
safetail xs = tail xs

--ex12
      --a
curta :: [a] -> Bool
curta xs = length xs < 3

      --b
curta1 :: [a] -> Bool
curta1 [] = True
curta1 (x:[]) = True
curta1 (x:y:[]) = True
curta1 _ = False

--ex13
digito2string :: Int -> String
digito2string n =  if n==0 then "zero" else
                    if n==1 then "um" else
                     if n ==2 then "dois" else
                      if n==3 then "tres" else
                       if n==4 then "quatro" else
                        if n==5 then "cinco" else
                          if n==6 then "seis" else
                            if n==7 then "sete" else
                             if n==8 then "oito" else
                               if n==9 then "nove" else "Por favor introduza um numero de 0 a 9"


doisdigitos2string:: Int -> String
doisdigitos2string n = if n == 10 then "dez" else
                    if n == 11 then "onze" else
                      if n == 12 then "doze" else
                        if n == 13 then "treze" else
                          if n == 14 then "catorze" else
                            if n == 15 then "quinze" else
                              if n == 16 then "dezaseis" else
                                if n == 17 then "dezassete" else
                                  if n == 18 then "dezoito" else
                                    if n == 19 then "dezanove" else
                                      if n == 20 then "vinte" else
                                        if n == 30 then "trinta" else
                                          if n == 40 then "quarenta" else
                                            if n == 50 then "cinquenta" else
                                              if n == 60 then "sessenta" else
                                                if n == 70 then "setenta" else
                                                  if n == 80 then "oitenta" else
                                                    if n == 90 then "noventa" else "Pr favor Introduza um numero de 10 a 19 ou um multiplo de 10 inferior a 100"

textualdoisdigitos:: Int -> String
textualdoisdigitos n = if n<10 then digito2string n else
                        if n<20 then doisdigitos2string n else
                          if n `mod` 10==0 then doisdigitos2string n else doisdigitos2string ((n `div` 10)*10) ++ " e " ++ digito2string (n `mod` 10)

tresdigitos2string :: Int -> String
tresdigitos2string n = if n==100 then "cem" else
                        if n==200 then "duzentos" else
                         if n ==300 then "trezentos" else
                          if n==400 then "quatrocentos" else
                           if n==500 then "quinhentos" else
                            if n==600 then "seiscentos" else
                             if n==700 then "setecentos" else
                              if n==800 then "oitocentos" else
                               if n==900 then "novecentos" else "Por favor introduza um multiplo de 100 inferior a 1000"



textualtresdigitos :: Int -> String
textualtresdigitos n = if n<100 then textualdoisdigitos n else
                         if n == 100 then "cem" else
                          if n<200 then "cento e " ++ textualdoisdigitos (n `mod` 100) else
                           if n `mod` 100 == 0 then tresdigitos2string n else
                            if n < 1000 then tresdigitos2string ((n `div` 100)*100) ++ " e " ++ textualdoisdigitos (n `mod` 100) else "Introduza um numero inferior a 1000"


textual :: Int -> String
textual n = if n < 1000 then textualtresdigitos n else
              if n == 1000 then "mil" else
                if n < 1101 then "mil e " ++ textualtresdigitos (n `mod` 1000) else
                 if n<1999 then "mil " ++ textualtresdigitos (n `mod` 1000) else
                   if n < 10000 then textualdoisdigitos (n `div` 1000) ++ " mil " ++ textualtresdigitos (n `mod` 1000) else "Introduza numeros até 9999"
