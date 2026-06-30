-- [let square x = x * x in (square 5, square 3, square 2)] 

lista = [1,1,1,2,2,1,1,3,3,3,1]
-- = [[1,1,1], [2,2], [1,1], [3,3,3], [1]]
agrupar :: Eq a => [a]  -> [[a]] -- junta en sublistas los a consecutivos que sean iguales
agrupar []     = []
agrupar [x] = [[x]]
agrupar (x:xs) = case agrupar xs of
                    [] -> [[x]]
                    (xs':xss') -> if x == head xs'
                                then (x : xs') : xss'
                                else [x] : (xs' : xss')

-- similar, solo que en vez de repetir el elemento en cada sublista, se queda con el elemento y dice cuantas veces se repite
comprimir :: Eq a => [a] -> [(Int, a)]
comprimir xs = map (\elem -> ( length elem , head elem)) (agrupar xs)

descomprimir :: [[a]] -> [a]

-- descoprimir' :: [(Int, a)] -> [a]