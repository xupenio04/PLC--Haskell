data Animal = Cisnal | Iguanoide | Narvale | Null
  deriving (Eq, Show)

dna2 :: [String] -> [String] -> [[Int]]
dna2 [] [] = [[0,0,0]]
dna2 [] _ = [[0,0,0]]
dna2 _ [] = [[0,0,0]]
dna2 (start1:end1) (start2:end2) =
    get_animals (divide_major start1 start2 (count_equals start1 start2 0))
                [0,0,0] : dna2 end1 end2
            
sum_all:: [[Int]]->[Int]
sum_all[] = [0,0,0]
sum_all (start:end) = foldr1 (zipWith (+)) (start:end)

finish_dna2:: [String]->[String]->[Int]
finish_dna2 [] [] =[0,0,0]
finish_dna2 [] (start2:end2)=[0,0,0]
finish_dna2 (start1:end1) []=[0,0,0]
finish_dna2 (start1:end1) (start2:end2) = sum_all(dna2(start1:end1)(start2:end2))

compare_char :: Char -> Char -> Bool
compare_char c1 c2 = c1 == c2

count_equals :: String -> String -> Float -> Float
count_equals [] [] n = n
count_equals [] _ n = n
count_equals _ [] n = n
count_equals (start1:end1) (start2:end2) n
    | compare_char start1 start2 = count_equals end1 end2 (n+1)
    | otherwise = count_equals end1 end2 n

divide_major :: String -> String -> Float -> Float
divide_major start1 start2 n
    | len1 >= len2 = n / len1
    | otherwise = n / len2
    where
        len1 = fromIntegral $ length start1
        len2 = fromIntegral $ length start2

get_animals :: Float -> [Int] -> [Int]
get_animals n [v1,v2,v3]
    | n >= 0.1 && n <= 0.3 = [v1+1,v2,v3]
    | n >= 0.4 && n <= 0.7 = [v1,v2+1,v3]
    | n >= 0.8             = [v1,v2,v3+1]
    | otherwise            = [v1,v2,v3]

main :: IO ()
main = do
    firstExtract <- words <$> getLine
    secondExtract <- words <$> getLine
    let result = finish_dna2 firstExtract secondExtract
    print result