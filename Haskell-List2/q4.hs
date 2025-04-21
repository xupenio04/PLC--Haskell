data Tree t = Node t (Tree t) (Tree t) | Nilt
  deriving (Read, Show)

get_full :: Tree Int -> String
get_full Nilt = ""
get_full (Node value tree1 tree2) = (get_full tree1) ++ [get_letter (value `mod` 5)] ++  get_full (tree2)

get_letter :: Int -> Char
get_letter n | n == 0 = 'E'
             | n == 1 = 'M'
             | n == 2 = 'A'
             | n == 3 = 'C'
             | n == 4 = 'S'

just_8 :: String -> [String]
just_8 [] = []
just_8 str
  | length str <= 8 = [str]
  | otherwise = take 8 str : just_8 (drop 8 str)

dna1 :: Tree Int -> [String]
dna1 Nilt = []
dna1 tree = just_8 (get_full tree)

main :: IO ()
main = do
  input <- getLine
  let result = dna1 (read input :: Tree Int)
  print result