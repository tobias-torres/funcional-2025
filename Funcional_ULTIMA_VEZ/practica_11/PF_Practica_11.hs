data Pizza = Prepizza | Capa Ingrediente Pizza deriving(Show)

data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamon | Queso | Salsa deriving (Show)

pzProof = Capa Queso (Capa Anchoas Prepizza)

-- ### Ejercicio 1

cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen p Prepizza    = 0
cantidadCapasQueCumplen p (Capa i pz) = sumaSiCumple p i + cantidadCapasQueCumplen p pz

sumaSiCumple :: (Ingrediente -> Bool) -> Ingrediente -> Int
sumaSiCumple f i = if f i then 1 else 0

conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas f Prepizza    = Prepizza
conCapasTransformadas f (Capa i pz) = Capa (f i) (conCapasTransformadas f pz)



soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue p Prepizza    = Prepizza
soloLasCapasQue p (Capa i pz) = armarSiCumple p i (soloLasCapasQue p pz)

armarSiCumple :: (Ingrediente -> Bool) -> Ingrediente -> Pizza -> Pizza
armarSiCumple f i pz = if f i then Capa i pz else pz

-- ### Ejercicio 2

-- sinLactosa :: Pizza -> Pizza
-- sinLactosa = soloLasCapasQue (\i -> not (esQueso i))

sinLactosa :: Pizza -> Pizza
sinLactosa = soloLasCapasQue (not . esQueso) 

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _     = False

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa =  ((==0) . cantidadCapasQueCumplen esQueso)
-- aptaIntolerantesLactosa pz = cantidadCapasQueCumplen (esQueso) pz == 0

cantidadDeQueso :: Pizza -> Int
cantidadDeQueso = cantidadCapasQueCumplen esQueso

conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas = conCapasTransformadas dupAceitunas

dupAceitunas :: Ingrediente -> Ingrediente
dupAceitunas (Aceitunas n ) = Aceitunas (2 * n)
dupAceitunas i              = i

-- ### Ejercicio 3

pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b
pizzaProcesada fp fpp Prepizza   = fpp
pizzaProcesada fp fpp (Capa i p) = fp i (pizzaProcesada fp fpp p)

-- ### Ejercicio 4

cantidadCapasQueCumplen' :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen' p = pizzaProcesada (\i n -> if p i then 1 + n else n) 0

conCapasTransformadas' :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas' f = pizzaProcesada (\i pz -> Capa (f i) pz) Prepizza

soloLasCapasQue' :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue' p = pizzaProcesada (\i pz -> if p i then Capa i pz else pz) Prepizza

sinLactosa'' :: Pizza -> Pizza
sinLactosa'' = pizzaProcesada (\i pz -> if not (esQueso i) then Capa i pz else pz) Prepizza

aptaIntolerantesLactosa' :: Pizza -> Bool
aptaIntolerantesLactosa' = pizzaProcesada (\i b -> not (esQueso i) && b) True

cantidadDeQueso' :: Pizza -> Int
cantidadDeQueso' = pizzaProcesada (\i n -> (cantQueso i) + n) 0

cantQueso :: Ingrediente -> Int
cantQueso Queso = 1
cantQueso _     = 0

conElDobleDeAceitunas' :: Pizza -> Pizza
conElDobleDeAceitunas' = pizzaProcesada (\i pz -> Capa (dupAceitunas i) pz) Prepizza


-- ## Ejercicio 5

pzProofDos = Capa Queso (Capa Anchoas (Capa (Aceitunas 20) (Capa (Aceitunas 10) Prepizza)))

cantidadAceitunas :: Pizza -> Int
cantidadAceitunas = pizzaProcesada (\i n -> cantAceituna i + n ) 0

cantAceituna :: Ingrediente -> Int
cantAceituna (Aceitunas n) = n
cantAceituna _             = 0

capasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplen f = pizzaProcesada (\i xs -> if f i then i : xs else xs) []

capasQueCumplen' :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplen' f Prepizza   = []
capasQueCumplen' f (Capa i p) = if f i 
        then i : (capasQueCumplen' f p) else capasQueCumplen' f p

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada = pizzaProcesada (\i pz -> juntarCapas i pz) Prepizza

juntarCapas :: Ingrediente -> Pizza -> Pizza
juntarCapas (Aceitunas n) (Capa (Aceitunas m) pz) = Capa (Aceitunas (n + m)) pz
juntarCapas i pz                                  = Capa i pz

conCapasDe :: Pizza -> Pizza -> Pizza
conCapasDe Prepizza pz2     = pz2
conCapasDe (Capa i pz1) pz2 = Capa i (conCapasDe pz1 pz2)

conCapasDe' :: Pizza -> Pizza -> Pizza
conCapasDe' pz1 pz2 = pizzaProcesada (\i pz -> Capa i pz) pz2 pz1

primerasNCapas :: Int -> Pizza -> Pizza
primerasNCapas 0 Prepizza    = Prepizza
primerasNCapas 0 pz          = pz
primerasNCapas n (Capa i pz) = primerasNCapas (n-1) pz

primerasNCapas' :: Int -> Pizza -> Pizza
primerasNCapas' = flip (pizzaProcesada (\i h n -> if n == 0 then Prepizza else Capa i (h (n-1))) 
                                       (const Prepizza))

take :: Int -> [a] -> [a]
take = flip (foldr (\x h n-> if n==0 then [] else x : h (n-1)) (\n-> []))


-- ### Ejercicio 7

map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : (map' f xs)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p []     = []
filter' p (x:xs) = if p x then x : (filter' p xs) else filter' p xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z []     = z
foldr' f z (x:xs) = f x (foldr' f z xs)

recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr z f []     = z
recr z f (x:xs) = f x xs (recr z f xs)

foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f [x]    = x
foldr1' f (x:xs) = f x (foldr1' f xs)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] ys         = []
zipWith' f xs []         = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- scanr' :: (a -> b -> b) -> b -> [a] -> [b]
-- scanr' f z []     = [z]
-- scanr' f z (x:xs) = let resto = scanr' f z xs
--                         in f x (head resto) : (scanr' f z xs)


-- ## Ejercicio 9

sum' :: [Int] -> Int
sum' = foldr (+) 0
-- sum' = foldr (\x n -> x + n) 0

length' :: [a] -> Int
length' = foldr (const (+1)) 0
-- length' = foldr (\x n -> 1 + n) 0

mapC :: (a -> b) -> [a] -> [b]
mapC f = foldr ((:) . f) []
-- mapC f = foldr (\x xs -> f x : xs) []

filterC :: (a -> Bool) -> [a] -> [a]
-- filterC p = foldr ()
filterC p = foldr (\x xs -> if p x then x : xs else xs) []

findC :: (a -> Bool) -> [a] -> Maybe a
findC f = foldr (\x m -> if f x then Just x else m) Nothing

anyC :: (a -> Bool) -> [a] -> Bool
anyC f = foldr (\x b -> f x || b) False 

allC :: (a -> Bool) -> [a] -> Bool
allC f = foldr (\x b -> f x && b) True

countBy :: (a -> Bool) -> [a] -> Int
countBy f = foldr (\x n -> if f x then 1 + n else n) 0


partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f = foldr (\x par -> if f x then (x : fst par, snd par) else (fst par, x : snd par)) ([],[])

-- zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith' f [] ys         = []
-- zipWith' f xs []         = []
-- zipWith' f (x:xs) (y:ys) = f x y : (zipWith' f xs ys)

zipWithC :: (a -> (b -> c)) -> ([a] -> ([b] -> [c]))
zipWithC f xs ys = foldr g z xs ys      -- aca foldr g z xs :: [b] -> [c], me hace falta pasarle la lista ys para que me de bien el tipo
                where z ys         = [] -- este es el caso base
                      g x r []     = [] -- esta seria la segunda ecuacion
                      g x r (y:ys) = f x y : r ys -- esta la tercera ecuacion



scanrC :: (a -> b -> b) -> b -> [a] -> [b]
scanrC f z = foldr (\x r -> f x (head r) : r) [z]
-- scanrC f z = foldr ((:) . (f head)) [z]

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f []     = []
takeWhile' f (x:xs) = if f x then x : takeWhile' f xs else []

takeWhileC :: (a -> Bool) -> [a] -> [a]
takeWhileC f = foldr (\x r -> if f x then x : r else []) []
-- takeWhileC f = foldr ((:) . )

take' :: Int -> [a] -> [a] 
take' n []     = []
take' 0 xs     = []
take' n (x:xs) = x : take' (n - 1) xs

takeL :: Int -> [a] -> [a]
takeL = flip (foldr (\x xs n -> if n == 0 then [] else x : xs (n-1)) (const []))

takeC :: Int -> [a] -> [a]
takeC n xs = foldr g z xs n
                where z n = []                  -- CASO BASE : take' n []     = []
                      g x r 0 = []              -- CASO 1.   : take' 0 xs     = []
                      g x r n = x : r (n-1)     -- CASO 2.   : take' n (x:xs) = x : take' (n - 1) xs

drop' :: Int -> [a] -> [a]
drop' n []     = []
drop' 0 xs     = xs
drop' n (x:xs) = drop' (n-1) xs 

dropC :: Int -> [a] -> [a]
dropC n xs = recr z g xs n
                where z n     = []
                      g x xs r 0 = x : xs
                      g x xs r n = r (n-1)

elemAt' :: Int -> [a] -> a
elemAt' n []     = error "no hay elementos para sacar" 
elemAt' 0 (x:xs) = x
elemAt' n (x:xs) = elemAt' (n-1) xs

elemAtL :: Int -> [a] -> a
elemAtL = flip (foldr (\x xs n -> if n == 0 then x else xs (n-1)) (const (error "no hay elementos para sacar")))

elemAt :: Int -> [a] -> a
elemAt n xs = foldr g z xs n
                where z n     = error "no hay elementos para sacar"
                      g x e 0 = x
                      g x e n = e (n-1)

-- ## Ejercicio 10

-- filter :: (a -> Bool) -> ([a] -> [a]) 
-- id :: a -> a                                 a -> Bool
-- filter id :: [Bool] -> [Bool]

-- map :: (a -> b) -> [a] -> [b] 
-- (\x y z -> (x, y, z)) :: a -> b -> c -> (a, b, c)
-- ---------------------------------------------------     a -> a, b -> (c -> (a, b, c))
-- map (\x y z -> (x, y, z)) :: [a] -> [b -> c -> (a, b, c)]

-- map :: (a -> b) -> [a] -> [b] 
-- (+) :: Int -> (Int -> Int)
----------------------------- 
-- map (+) :: [Int] -> [Int -> Int]

-- filter :: (a -> Bool) -> ([a] -> [a]) 
-- fst :: (a,b) -> a
-- ------------------------------------- a -> (a, b), Bool -> a
-- filter fst :: [(Bool, a)] -> [(Bool, a)]

-- filter (flip const (+))

-- flip :: (b -> a -> c) -> a -> b -> c
-- const :: a -> b -> a
-- (+) :: Int -> Int -> Int

-- map :: (a -> b) -> [a] -> [b] 
-- const :: a -> b -> a
-- map const :: [a] -> [b -> a]

-- map twice :: [a -> a] -> [a -> a]

-- foldr twice