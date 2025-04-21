data Tree t = Node t (Tree t) (Tree t) 
              | Nilt
              deriving (Read)
              
maxi:: Int->Int ->Int
maxi n m  | m>=n = m
          | otherwise = n  
              
depth :: Tree t -> Int
depth Nilt = 0
depth (Node value (tree1) (tree2)) = 1 + maxi (depth (tree1))(depth (tree2))

main = do
       a <- getLine
       let result = depth (read a::Tree Int)
       print result