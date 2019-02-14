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
