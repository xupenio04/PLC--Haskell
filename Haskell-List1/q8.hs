addespacos:: Int-> String
addespacos n  | n==0 =""
              | n==1 =" "
              | otherwise = " "++(addespacos(n-1))
paraDireita:: Int->String->String
paraDireita n word = addespacos(n) ++ word


parseInput str = let [n, s] = words str
                 in (read n, s)
main :: IO()
main = interact $ uncurry paraDireita . parseInput