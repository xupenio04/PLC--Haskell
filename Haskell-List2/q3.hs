uncommonFromTwoSentences:: String -> String-> [String]
uncommonFromTwoSentences [] [] =[]
uncommonFromTwoSentences (start1:end1) (start2:end2) = get_unique(help_string(qSort(split_word(lower_letter(start1:end1)) ++ split_word(lower_letter(start2:end2))))(1))


lower_letter:: String->String
lower_letter [] =[]
lower_letter (start:end)=to_lower(start):lower_letter(end)

split_word :: String -> [String]
split_word (start:end) = words(start:end)

to_lower:: Char->Char
to_lower c | c>='A' && c<='Z' = toEnum(fromEnum(c)+32)
           | otherwise = c

get_unique:: [(String,Int)]->[String]
get_unique [] =[]
get_unique (start:end) |snd(start)==1 = fst(start):get_unique(end)
                       | otherwise = get_unique(end)
qSort :: [String] -> [String]
qSort [] = []
qSort (x:xs) =qSort [y | y <- xs, y < x] ++ [x] ++ qSort [y | y <- xs, y >= x]

help_string:: [String]-> Int->[(String,Int)]
help_string [] _ = [("",0)]
help_string [v1] n = [(v1,n)]
help_string (v1:v2:end) n  | (v1==v2)  = help_string(v2:end)(n+1)
                           | otherwise = (v1,n):help_string(v2:end)(1) 
main = do
sentence_1 <- getLine
sentence_2 <- getLine
let result = uncommonFromTwoSentences sentence_1 sentence_2
print result