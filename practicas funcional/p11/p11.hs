import Prelude hiding (map, filter, foldr, recr, foldr1, zipWith, scanr, length,any, all, countBy, partition, zipWith, scanr, takeWhile, take, drop, elemAt)

data Pizza = Prepizza | Capa Ingrediente Pizza deriving (Show)

data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamon | Queso | Salsa deriving (Show)

pz1 = Capa Salsa (Capa Anchoas Prepizza) 
pz2 = Capa Cebolla (Capa Queso (Capa Jamon Prepizza))

cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int 
cantidadCapasQueCumplen f Prepizza   = 0
cantidadCapasQueCumplen f (Capa i p) = sumaSiCumple f i + cantidadCapasQueCumplen f p

sumaSiCumple :: (Ingrediente -> Bool) -> Ingrediente -> Int
sumaSiCumple f i = if f i then 1 else 0

conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas f Prepizza   = Prepizza
conCapasTransformadas f (Capa i p) = Capa (f i) (conCapasTransformadas f p)

soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue f Prepizza   = Prepizza
soloLasCapasQue f (Capa i p) = armarSiCumple f i (soloLasCapasQue f p)

armarSiCumple :: (Ingrediente -> Bool) -> Ingrediente -> Pizza -> Pizza
armarSiCumple f i p = if f i then Capa i p else p

-- Ejercicio 2
sinLactosa :: Pizza -> Pizza 
sinLactosa = soloLasCapasQue (not . esQueso)


-- compose :: (b -> c) -> (a -> b) -> a -> c
-- not     :: Bool -> Bool
-- -----------------------------------------
-- compose not :: (a -> Bool) -> a -> Bool
-- esQueso     :: Ingrediente -> Bool
-- -----------------------------------------
-- (not . esQueso) :: Ingrediente -> Bool

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _     = False

aptaIntolerantesLactosa :: Pizza -> Bool 
aptaIntolerantesLactosa = (==0) . cantidadCapasQueCumplen esQueso

cantidadDeQueso :: Pizza -> Int 
cantidadDeQueso = cantidadCapasQueCumplen esQueso

conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas = conCapasTransformadas duplicar

duplicar :: Ingrediente -> Ingrediente
duplicar (Aceitunas n) = Aceitunas (2 * n)
duplicar i             = i 

-- Ejercicio 3

pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b
pizzaProcesada f z Prepizza   = z
pizzaProcesada f z (Capa i p) = f i (pizzaProcesada f z p)

-- Ejercicio 4

cantidadCapasQueCumplen' :: (Ingrediente -> Bool) -> Pizza -> Int
-- cantidadCapasQueCumplen' f = pizzaProcesada (\i n -> sumaSiCumple f i + n) 0
-- cantidadCapasQueCumplen' f = pizzaProcesada (\i n -> (+) (sumaSiCumple f i) n) 0
-- cantidadCapasQueCumplen' f = pizzaProcesada (\i -> (+) (sumaSiCumple f i)) 0
cantidadCapasQueCumplen' f = pizzaProcesada ((+) . sumaSiCumple f) 0

conCapasTransformadas' :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
-- conCapasTransformadas' f = pizzaProcesada (\i p -> Capa (f i) p) Prepizza
-- conCapasTransformadas' f = pizzaProcesada (\i -> Capa (f i)) Prepizza
conCapasTransformadas' f = pizzaProcesada (Capa . f ) Prepizza

soloLasCapasQue' :: (Ingrediente -> Bool) -> Pizza -> Pizza
-- soloLasCapasQue' f = pizzaProcesada (\i p -> armarSiCumple f i p) Prepizza
-- soloLasCapasQue' f = pizzaProcesada (\i -> armarSiCumple f i) Prepizza
soloLasCapasQue' f = pizzaProcesada (armarSiCumple f) Prepizza

sinLactosa' :: Pizza -> Pizza 
sinLactosa' = pizzaProcesada (armarSiCumple (not . esQueso)) Prepizza

aptaIntolerantesLactosa' :: Pizza -> Bool
-- aptaIntolerantesLactosa' = pizzaProcesada (\i b -> not (esQueso i) && b ) True
-- aptaIntolerantesLactosa' = pizzaProcesada (\i b -> (&&) (not (esQueso i)) b) True
-- aptaIntolerantesLactosa' = pizzaProcesada (\i -> (&&) (not (esQueso i))) True
aptaIntolerantesLactosa' = pizzaProcesada ((&&) . not . esQueso) True

cantidadDeQueso' :: Pizza -> Int
-- cantidadDeQueso' = pizzaProcesada (\i n -> sumaSiCumple esQueso i + n) 0
-- cantidadDeQueso' = pizzaProcesada (\i n -> (+) (sumaSiCumple esQueso i) n) 0
-- cantidadDeQueso' = pizzaProcesada (\i -> (+) (sumaSiCumple esQueso i)) 0
cantidadDeQueso' = pizzaProcesada ((+) . sumaSiCumple esQueso) 0

conElDobleDeAceitunas' :: Pizza -> Pizza
-- conElDobleDeAceitunas' = pizzaProcesada (\i p -> Capa (duplicar i) p) Prepizza
-- conElDobleDeAceitunas' = pizzaProcesada (\i -> Capa (duplicar i)) Prepizza
conElDobleDeAceitunas' = pizzaProcesada (Capa . duplicar) Prepizza

-- Ejercicio 5

cantidadAceitunas :: Pizza -> Int 
-- cantidadAceitunas = pizzaProcesada (\i n -> nroAceitunas i + n) 0
cantidadAceitunas = pizzaProcesada ((+) . nroAceitunas) 0

nroAceitunas :: Ingrediente -> Int
nroAceitunas (Aceitunas n) = n
nroAceitunas _             = 0

capasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplen f = pizzaProcesada (\i xs -> if f i then i : xs else xs ) []

conDescripcionMejorada :: Pizza -> Pizza
-- conDescripcionMejorada = pizzaProcesada (\i p -> mejorar i p) Prepizza
-- conDescripcionMejorada = pizzaProcesada (\i -> mejorar i) Prepizza
conDescripcionMejorada = pizzaProcesada mejorar Prepizza

mejorar :: Ingrediente -> Pizza -> Pizza
mejorar (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n + m)) p
mejorar i p = Capa i p

-- conCapasDe :: Pizza -> Pizza -> Pizza
-- conCapasDe Prepizza pz   = pz
-- conCapasDe (Capa i p) pz = Capa i (conCapasDe p pz)

conCapasDe :: Pizza -> Pizza -> Pizza
-- conCapasDe pz = pizzaProcesada (\i p -> Capa i p) pz
conCapasDe pz = pizzaProcesada Capa pz

primerasNCapas :: Int -> Pizza -> Pizza
primerasNCapas = flip (pizzaProcesada (\i p n -> if n == 0 then Prepizza else Capa i (p (n-1))) 
                                      (const Prepizza))

-- Ejercicio 7

map :: (a -> b) -> [a] -> [b] 
map f []     = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a] 
filter p []     = []
filter p (x:xs) = if p x then x : filter p xs else filter p xs

foldr :: (a -> b -> b) -> b -> [a] -> b 
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

recr :: b -> (a -> [a] -> b -> b) -> [a] -> b 
recr z f []     = z
recr z f (x:xs) = f x xs (recr z f xs)

foldr1 :: (a -> a -> a) -> [a] -> a 
foldr1 f [x]    = x
foldr1 f (x:xs) = f x (foldr1 f xs)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] ys     = []
zipWith f xs []     = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

-- scanr :: (a -> b -> b) -> b -> [a] -> [b]
-- scanr f z []     = [z]
-- scanr f z (x:xs) = f x ( head rs ) : rs
--   where rs = scanr f z xs

-- Ejercicio 9

sum :: [Int] -> Int 
sum = foldr (+) 0

length :: [a] -> Int 
-- length = foldr (\x n -> 1 + n) 0
-- length = foldr (\x n -> (+) 1 n) 0
-- length = foldr (\x -> (+) 1) 0
length = foldr (const (+1)) 0

map' :: (a -> b) -> [a] -> [b] 
-- map' f = foldr (\x xs -> f x : xs) []
-- map' f = foldr (\x xs -> (:) (f x) xs) []
-- map' f = foldr (\x -> (:) (f x)) []
map' f = foldr ((:) . f) []

filter' :: (a -> Bool) -> [a] -> [a] 
filter' p = foldr (\x xs -> if p x then x : xs else xs) []

find' :: (a -> Bool) -> [a] -> Maybe a 
find' p = foldr (\i m -> if p i then Just i else m ) Nothing

any :: (a -> Bool) -> [a] -> Bool 
any p = foldr ((||) . p) False

all :: (a -> Bool) -> [a] -> Bool 
all p = foldr ((&&) . p) True

countBy :: (a -> Bool) -> [a] -> Int 
countBy p = foldr (\i n -> if p i then 1 + n else n) 0

partition :: (a -> Bool) -> [a] -> ([a], [a]) 
partition p = foldr (\x par -> if p x then (x : fst par, snd par) else (fst par, x : snd par)) ([],[])

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c] 
zipWith' f = foldr (\x xs ys -> case ys of 
                                [] -> []
                                (y:ys') -> f x y : xs ys') (const [])


-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith f [] ys     = []
-- zipWith f xs []     = []
-- zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

-- scanr :: (a -> b -> b) -> b -> [a] -> [b] 
-- takeWhile :: (a -> Bool) -> [a] -> [a] 
-- take :: Int -> [a] -> [a] 
-- drop :: Int -> [a] -> [a] 
-- -- elemAt :: Int -> [a] -> a

-- Ejercicio 10

-- a.  filter id :: [Bool] -> [Bool]

-- b.  map (\x y z -> (x, y, z))

-- map                       :: (a -> b) -> [a] -> [b]
-- (\x y z -> (x, y, z))     :: a -> b -> c -> (a, b, c)
-- ---------------------------------------------------------
-- map (\x y z -> (x, y, z)) :: [a] -> [b -> c -> (a, b, c)]


-- c.  map (+) :: [Int] -> [Int] -> [Int]

-- map     :: (a -> b) -> [a] -> [b]
-- (+)     :: Int -> (Int -> Int)
-- ---------------------------------------------------------
-- map (+) :: [Int] -> [Int -> Int]


-- d.  filter fst 
filter     :: (a -> Bool) -> [a] -> [a]
fst        :: (a, b) -> a
--------------------------------------------------------- a -> (a, b), Bool -> a
filter fst :: [(Bool, b)] -> [(Bool, b)]


-- e.  filter (flip const (+))
filter                  :: (a -> Bool) -> [a] -> [a]
(flip const (+))        :: 
--------------------------------------------------------- 
filter (flip const (+)) :: 


-- flip  :: (a -> b -> c) -> a -> c -> b
-- const :: a -> b -> a
-- (+)   :: Int -> (Int -> Int)

-- f.  map const 
-- g.  map twice 
-- h.  foldr twice 
-- i.  zipWith fst 
-- j.  foldr (\x r z -> (x, z) : r z) (const []) 
