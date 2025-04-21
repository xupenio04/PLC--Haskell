suaviza :: [Float] -> [Float]
suaviza [] = []
suaviza [v1]=[v1]
suaviza [v1,v2]=[v1,v2]
suaviza (v1:v2:v3:end) =  v1:get_empty(v1:v2:v3:end)


get_empty :: [Float] -> [Float]
get_empty [] = []
get_empty [v1] = [v1]
get_empty [v1, v2] = [v1, v2]
get_empty [v1, v2, v3] = [(v1 + v2 + v3) / 3, v3]
get_empty (v1:v2:v3:end) = (v1 + v2 + v3) / 3: get_empty(v2:v3:end)


main = do
        lista <- getLine
        print $ suaviza (read lista :: [Float])