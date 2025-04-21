lista_menor::String->[String]->[String]
lista_menor _ [] =[]
lista_menor p (start:end)      | (p>=start)         = start:lista_menor(p)(end)
                               | otherwise          = lista_menor(p)(end)
                                
lista_maior::String->[String]->[String]
lista_maior _ [] =[]
lista_maior p (start:end)    | (p<start)        = start:lista_maior(p)(end)
                             | otherwise         = lista_maior(p)(end)
                             
quick_sort::[String]->[String]
quick_sort [] =[]
quick_sort (start:end)  = quick_sort(lista_menor(start)(end)) ++ [start] ++ quick_sort(lista_maior(start)(end))

main = do
       a <- getLine
       let result = quick_sort (read a :: [String])
       print result