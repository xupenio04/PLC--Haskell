fat_prime :: Int -> [(Int, Int)]
fat_prime n = count_primes(prime_factors(n))

is_prime :: Int -> Bool
is_prime 2 = True
is_prime n = modl n (div n 2 +1)
    where modl x 2 = mod x 2 == 1
          modl x y | mod x y == 0 = False
                   | otherwise = modl x (y-1)

prime_factors :: Int -> [Int]
prime_factors n
    | n <= 1 = []
    | otherwise = factor : prime_factors (n `div` factor)
    where
        factor = head [x | x <- [2..n], n `mod` x == 0, is_prime x]
        

count_primes:: [Int]->[(Int,Int)]
count_primes [] =[]
count_primes (start:end) = help_primes(start:end)(1)

help_primes:: [Int]-> Int->[(Int,Int)]
help_primes [] _ = [(0,0)]
help_primes [v1] n = [(v1,n)]
help_primes (v1:v2:end) n  | (v1==v2)  = help_primes(v2:end)(n+1)
                           | otherwise = (v1,n):help_primes(v2:end)(1)   
 

main = do
      a <- getLine
      let result = fat_prime (read a :: Int)
      print result