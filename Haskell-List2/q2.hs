count_string:: [String]->[String]
count_string [] =[]
count_string (start:end) = get_three(remove_equals(sort_list2(sort_list(help_string(qSort(start:end))(1)))))

help_string:: [String]-> Int->[(String,Int)]
help_string [] _ = [("",0)]
help_string [v1] n = [(v1,n)]
help_string (v1:v2:end) n  | (v1==v2)  = help_string(v2:end)(n+1)
                           | otherwise = (v1,n):help_string(v2:end)(1) 

sort_list::[(String,Int)]->[(String,Int)]
sort_list []=[]
sort_list (start:end) = sort_list [y | y <- end, length(fst(y)) < length(fst(start))] ++ [start] ++ sort_list [y | y <- end, length(fst(y))  >= length(fst(start))]

maxi:: Int->Int ->Int
maxi n m  | m>=n = m
          | otherwise = n 

sort_list2::[(String,Int)]->[(String,Int)]
sort_list2 []=[]
sort_list2 (start:end) = sort_list2 [y | y <- end, snd y > snd start] ++ [start] ++ sort_list2 [y | y <- end, snd y <= snd start]
   

remove_equals :: [(String,Int)] -> [(String,Int)]
remove_equals [] = []
remove_equals (start:end) = start : remove_equals (filter (/=start) end)

get_three:: [(String,Int)] ->[String]
get_three []=[]
get_three [v1] = [fst(v1)]
get_three [v1,v2]= [fst(v1),fst(v2)]
get_three [v1,v2,v3] = [fst(v1),fst(v2),fst(v3)]  
get_three (v1:v2:v3:end)= [fst(v1),fst(v2),fst(v3)] 
              
qSort :: [String] -> [String]
qSort [] = []
qSort (x:xs) =qSort [y | y <- xs, y < x] ++ [x] ++ qSort [y | y <- xs, y >= x]

main = do
        lista <- getLine
        print $ count_string (read lista :: [String])