-- f []     =
-- f (x:xs) = ... f xs
import Prelude hiding ((++), reverse, length, sum, product, concat, elem, all, any, count, subset, zip, unzip)

lista = [[23,33],[1],[45,66]]
listaDos = [1,2,3,4,5,66]

-- a. 
length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs

-- b. 
sum' :: [Int] -> Int
sum' []     = 0
sum' (x:xs) = x + sum' xs

-- c. que describe el producto entre todos los elementos de la lista
product' :: [Int] -> Int
product' []     = 1
product' (x:xs) = x * product' xs

-- -- d. que describe la lista resultante de concatenar todas las listas que son elementos de la dada.
concat' :: [[a]] -> [a]
concat' []       = []
concat' (xs:xss) = xs ++ concat' xss

-- -- e.  que indica si el elemento dado pertenece a la lista.
elem :: Eq a => a -> [a] -> Bool
elem e []     = False
elem e (x:xs) = e == x || elem e xs

-- -- f. , que indica si todos los elementos de la lista cumplen el predicado dado.
all' :: (a -> Bool) -> [a] -> Bool
all' f []     = True
all' f (x:xs) = f x && all' f xs

funcionLoca = \x -> 10 > x

-- -- g. 
-- , que indica si algún elemento de la lista cumple el predicado dado.
any' :: (a -> Bool) -> [a] -> Bool
any' f []     = False
any' f (x:xs) = f x || any' f xs

-- -- h. 
-- , que describe la cantidad de elementos de la lista que cumplen el predicado dado.
count' :: (a -> Bool) -> [a] -> Int
count' f []     = 0
count' f (x:xs) = sumarSiCumple f x + count' f xs

sumarSiCumple :: (a -> Bool) -> a -> Int
sumarSiCumple f x = if f x then 1 else 0

-- -- i. 
--  que indica si todos los elementos de la primera lista se encuentran en la segunda.
subset' :: Eq a => [a] -> [a] -> Bool
subset' [] ys     = True
subset' (x:xs) ys = elem x ys && subset' xs ys

-- -- j. 
-- , que describe el resultado de agregar los elementos de la primera lista adelante de los elementos de la segunda.
(++) :: [a] -> [a] -> [a]
(++) []     ys = ys
(++) (x:xs) ys = x : (xs ++ ys)

-- k. 
-- que describe la lista que tiene los elementos en el orden inverso a la lista dada.
reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

-- -- l. que describe la lista resultante de juntar de a pares los elementos  de  ambas  listas, según  la posición que comparten en cada una
zip :: [a] -> [b] -> [(a,b)]
zip [] ys         = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys

-- -- m. 
-- , que describe el par de listas que
-- resulta de desarmar la lista dada; la primera componente del resultado se
-- corresponde con las primeras componentes de los pares dados, y la segunda
-- componente con las segundas componentes de dichos pares.
unzip :: [(a,b)] -> ([a],[b])
unzip []     = ([],[])
unzip (x:xs) = (fst x : fst (unzip xs), snd x : snd (unzip xs))

