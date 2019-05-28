import Data.Char
--ex65
elefantes :: Int -> IO ()
elefantes n
  | n < 2 = return ()
  |otherwise = sequence_ [putStrLn(verso x)|x<-[2..n]]

verso :: Int -> String
verso n = "Se " ++ show n ++ " elefantes incomodam muita gente, \n" ++ show (n+1) ++ " elefantes incomodam muito mais!"

--ex66
{-
wc :: String -> String
wc s = show (length (lines s)) ++ "\t"
       show (length (words s)) ++ "\t"
       show (length s)

main = do {
          s <- getLine;
          let s' = wc s in
          putStrLn s';
}
-}
--ex67
{-
main = rev(do {
                s <- getLine;
                putStrLn (reverse s);
})
-}
