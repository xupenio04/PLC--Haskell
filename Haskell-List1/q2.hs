                   
maquinaSomar :: [Int] -> [Int]
maquinaSomar [] = []
maquinaSomar [0] = []
maquinaSomar (0:xs) | head xs == 0 = []
                    | otherwise = maquinaSomar xs
maquinaSomar xs = sum start : maquinaSomar end
    where (start, end) = span (/= 0) xs
   

main = do
       lista <- getLine
       print $ maquinaSomar (read lista :: [Int])