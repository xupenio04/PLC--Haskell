executa :: [(String, Int)] -> Int
executa [] = 0
executa (start:end) = executa_zero(start:end)(0)

executa_zero:: [(String,Int)]->Int->Int
executa_zero [] n = n
executa_zero (start:end) n  
  | fst(start)=="Divide" && snd(start)==0 = -666
  | otherwise = executa_zero(end)(for_zero(n)(start))

for_zero:: Int->(String, Int)-> Int
for_zero  n (v1,v2) 
       | (v1=="Multiplica")        = n*v2
       | (v1=="Soma")              = n+v2
       | (v1=="Subtrai")           = n-v2
       | (v1=="Divide") && (v2==0) = -666
       | (v1=="Divide") && (v2/=0) = n `div` v2 

main = do
    a <- getLine
    let result = executa (read a)
    print result