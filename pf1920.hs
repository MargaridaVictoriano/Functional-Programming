--ex7a
max3 :: Int -> Int -> Int -> Int
max3 a b c = if a > b && a > c then a else
             if b > a && b > c then b else c

min3 :: Int -> Int -> Int -> Int
min3 a b c = if a < b && a < c then a else
             if b < a && b < c then b else c

--ex7b
maxx :: Int -> Int -> Int -> Int
maxx a b c = max a (max b c)

minn :: Int -> Int -> Int -> Int
minn a b c = min a (min b c)

--ex8a
maxOccurs :: Integer -> Integer -> (Integer, Integer)
maxOccurs a b = (max a b, n)
                 where n = if a==b then 2 else 1

--ex8b
orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (x,y,z) = (a,b,c)
                      where a = min (min x y) z
                            b = if (a == x || a == z) && (c == x || c == z) then y else
                                  if (a == y || a == z) && (c == y || c == z) then x else z
                            c = max (max x y) z

--ex9
classifica :: Int -> String
classifica n
            |n < 10 = "Reprovado"
            |n < 13 = "suficiente"
            |n < 16 = "bom"
            |n < 19 = "muito bom"
            |n < 21 = "muito bom com distincao"
            |otherwise = "Valor invalido, tente de novo."

classifica1 :: Int -> String
classifica1 x = if x < 10 then "reprovado" else
                  if x < 13 then "suficiente" else
                    if x < 16 then "bom" else
                      if x < 19 then "muito bom" else
                        if x < 21 then "muito bom com distincao" else "valor invalido"

--ex10
xor :: Bool -> Bool -> Bool
xor True True = False 
xor False False = False
xor True False = True
xor False True = True

--ex11
safetail :: [a] -> [a]
safetail [] = []
safetail xs = tail xs

--ex12a
curta :: [a] -> Bool
curta xs = length xs < 3

--ex12b
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
