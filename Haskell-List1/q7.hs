mul2 :: [Int] -> [Int] -> [Int]
mul2 [] [] = []
mul2 [] (start2:end2)=[0]++mul2([])(end2)
mul2 (start1:end1) []=[0]++mul2(end1)([])
mul2 (start1:end1) (start2:end2) =  [(start1*start2)]++mul2(end1)(end2)


main = do
    sa <- getLine
    let a = read sa :: [Int]
    sb <- getLine
    let b = read sb :: [Int]
    let result = mul2 a b
    print result