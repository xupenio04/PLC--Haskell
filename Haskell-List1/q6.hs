minMaxCartao :: String -> (Double, Double)
minMaxCartao str = (head(min_max(str)),head(reverse((min_max(str)))) )
    
isnot_pv:: String -> [String]
isnot_pv ""= []
isnot_pv (';':end) = isnot_pv(end)
isnot_pv string = (start:isnot_pv(end))
    where (start,end)= span(/=';')(string)
    
get_doubles:: [String] ->[String]
get_doubles [] =[]
get_doubles (v1:v2:v3:end) =(v3:get_doubles(end))

string_double ::[String]->[Double]
string_double [] =[]
string_double (start:end) = read(start):(string_double(end)) 

min_max :: String->[Double]
min_max str = qSort(string_double((get_doubles(isnot_pv(str)))))

qSort :: [Double] -> [Double]
qSort [] = []
qSort (x:xs) =qSort [y | y <- xs, y < x] ++ [x] ++ qSort [y | y <- xs, y >= x]


main = do
    a <- getLine
    let result = minMaxCartao a
    print result