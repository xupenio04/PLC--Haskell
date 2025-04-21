

logMes :: String->String->Double
logMes str1 str2 = sum_all((just_values(str1)(form_tuple(get_values(splitStrings(get_doubles(isnot_pv(str2))))))))

isnot_pv:: String -> [String]
isnot_pv ""= []
isnot_pv (';':end) = isnot_pv(end)
isnot_pv string = (start:isnot_pv(end))
    where (start,end)= span(/=';')(string)
   
get_doubles:: [String] ->[String]
get_doubles [] =[]
get_doubles (v1:v2:v3:end) =(v1:v3:get_doubles(end))

not_space:: [String]->[String]
not_space [] =[]
not_space (start:end) | head(start)==' ' = start:not_space(end)
                      | otherwise = not_space(end)

splitPalavras :: String -> [String]
splitPalavras str = words str

splitStrings :: [String] -> [String]
splitStrings = concatMap splitPalavras

get_values:: [String]->[String]
get_values [] =[]
get_values (v1:v2:v3:end)= (v2:v3:get_values(end))

form_tuple:: [String]->[(String,Double)]
form_tuple []= [("",0.0)]
form_tuple (v1:v2:end) = (v1,read(v2)):form_tuple(end)

just_values:: String->[(String,Double)]->[Double]
just_values _ [] = []
just_values str ((v1,v2):end) | v1==str   = v2:just_values(str)(end)
                              | otherwise = just_values(str)(end)
 
sum_all::[Double]->Double
sum_all list = foldl(\acc x -> acc +x)(0.0)(list)
main = do
    a <- getLine
    b <- getLine
    let result = logMes a b
    print result