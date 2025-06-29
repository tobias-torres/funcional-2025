-- f []     = ...
-- f (x:xs) = ... f xs


lengthB :: [a] -> Int
lengthB []     = 0
lengthB (x:xs) = 1 + lengthB xs

-- Int, que describe la suma de todos los elementos de la lista.
sumB :: [Int] -> Int
sumB []     = 0
sumB (x:xs) = x + sumB xs

-- , que describe el producto entre todos los elementos de la lista.
productB :: [Int] -> Int
productB []     = 1
productB (x:xs) = x * (productB xs)

-- , que describe la lista resultante de concatenar todas las listas que son elementos de la dada.
concatB :: [[a]] -> [a]
concatB []       = []
concatB (xs:xss) = xs ++ concatB xss

-- , que indica si el elemento dado pertenece a la lista.
elemB :: Eq a => a -> [a] -> Bool
elemB e []     = False
elemB e (x:xs) = e == x || elemB e xs

-- , que indica si todos los elementos de la lista cumplen el predicado dado.
allB :: (a -> Bool) -> [a] -> Bool
allB f []     = True
allB f (x:xs) = f x && allB f xs

-- , que indica si algún elemento de la lista cumple el predicado dado.
anyB :: (a -> Bool) -> [a] -> Bool
anyB f []     = False
anyB f (x:xs) = f x || anyB f xs

-- , que describe la cantidad de elementos de la lista que cumplen el predicado dado.
countB :: (a -> Bool) -> [a] -> Int
countB f []     = 0
countB f (x:xs) = if (f x) then 1 + (countB f xs) else countB f xs

-- , que indica si todos los elementos de la primera lista se encuentran en la segunda.
subset :: Eq a => [a] -> [a] -> Bool
subset []     ys = True
subset (x:xs) ys = (elemB x ys) && subset xs ys

ls = [1,2,3,4,5]

-- que describe el resultado de agregar los elementos de la primera lista adelante de los elementos de la segunda.
-- (++) :: [a] -> [a] -> [a]
-- (++) []     ys = ys
-- (++) xs     [] = xs
-- (++) (x:xs) ys = x : (xs (++) ys)

-- que describe la lista que tiene los elementos en el orden inverso a la lista dada.
verse :: [a] -> [a]
verse []     = []
verse (x:xs) = (verse xs) ++ [x]

-- que describe la lista resultante de juntar de a pares los elementos de ambas listas, según la posición que comparten en cada una.
zipB :: [a] -> [b] -> [(a,b)]
zipB [] ys         = []
zipB xs []         = []
zipB (x:xs) (y:ys) = (x, y) : (zipB xs ys)

-- que describe el par de listas que resulta de desarmar la lista dada; la primera componente del resultado se
-- corresponde con las primeras componentes de los pares dados, y la segunda componente con las segundas componentes
-- de dichos pares.
unzipB :: [(a,b)] -> ([a],[b])
unzipB []     = ([],[])
unzipB (x:xs) = (fst x : fst (unzipB xs) , snd x : snd (unzipB xs))


-- [(1, 2), (3, 4), (5, 6)] -> ([1, 3, 5], [2, 4, 6]) 

-- (1, 2) 

-- Ejercicio 2) Demostrar por inducción estructural las siguientes propiedades:

-- a. para todo xs. para todo ys. length (xs ++ ys) = length xs + length ys?
-- Dem) sean zs y ws de tipo listas, por ppio de induccion estructural sobre la estructura de zs es equivalente demostrar que:

-- Caso Base zs = []

-- ¿len ([] ++ ws) = len [] + len ws?

-- Caso ind zs = (x:xs')

-- HI) length (xs' ++ ys) = length xs' + length ys !
-- TI) length ((x:xs') ++ ys) = length (x:xs') + length ys ?

---------------------------------------------------------------------------

-- Caso base:

-- LI)

-- length ([] ++ ws) 
--     ---------
-- =                                   def ++
-- length ws

-- LD)

-- length [] + length ws
-- ------
-- =                                   def length
-- 0 + length ws
-- ----------
-- =                                   cero es neutro de la suma
-- length ws

-- Caso Inductivo:

-- LI)

-- length ((x:xs') ++ ys)
--     --------------
-- =                                   def ++
-- length (x : (xs (++) ys))
-- ----------------------
-- =                                   def length
-- 1 + length (xs (++) ys)

-- LD) 

-- length (x:xs') + length ys
-- --------------
-- =                                   def length
-- 1 + length xs' + length ys
--     ----------------------
-- =                                   HI
-- 1 + length (xs (++) ys)

-- ---------------------------------------------------------------------------

-- -- b.
-- Prop: count (const True) = length
-- Dem: Por ppio de extensionalidad, es equivalente demostrar que
-- para todo xs. count (const True) xs = length xs
-- sea zs una lista finita, por ppio de induccion sobre la estructura de zs es equivalente demostrar que:

-- Caso Base: zs = [])

-- count (const True) [] = length [] ?

-- Caso Inductivo: zs = (x : xs')

-- HI) count (const True) xs' = length xs' !
-- TI) count (const True) (x:xs') = length (x:xs') ?

-- Caso Base:

-- LI)

-- count (const True) []
-- ---------------------
-- =                                   def count
-- 0

-- LD)

-- length []
-- ---------
-- =                                   def length
-- 0

-- Caso Inductivo:

-- LI)

-- count (const True) (x:xs')
-- --------------------------
-- =                                   def count
-- if (const True x) then 1 + (count (const True) xs') else count (const True) xs'
--     ------------
-- =                                   def const
-- 1 + (count (const True) xs')

-- LD)

-- length (x:xs')
-- --------------
-- =                                   def length
-- 1 + length xs'
--     ---------
-- =                                   HI
-- 1 + count (const True) xs'

-- countB :: (a -> Bool) -> [a] -> Int
-- countB f []     = 0
-- countB f (x:xs) = if (f x) then 1 + (countB f xs) else countB f xs

-- ---------------------------------------------------------------------------

-- -- c.
-- Prop: elem = any . (==)
-- Dem: Por ppio de extensionalidad
-- para todo x. elem x = (any . (==)) x
-- por def de (.)
-- para todo x. elem x = any ((==) x)
-- para todo x. para todo xs. elem x xs = any ((==) x) xs
-- sea e un elemento cualquiera y zs una lista finita, por ppio de induccion sobre la estructura de zs es equivalente

-- Caso Base: zs = []

-- elem e [] = any ((==) e) [] ?

-- Caso Inductivo: zs = (x: xs')

-- HI) elem e xs' = any ((==) e) xs' !
-- TI) elem e (x:xs') = any ((==) e) (x:xs') ?

-- Caso Base:

-- LI)

-- elem e []
-- ---------
-- =                                   def elem
-- False

-- LD)

-- any ((==) e) []
-- ---------------
-- =                                   def any
-- False


-- Caso Inductivo:

-- LI)

-- elem e (x:xs')
-- --------------
-- =                                   def elem
-- e == x || elem e xs'

-- LD)

-- any ((==) e) (x:xs')
-- -------------------
-- =                                   def any
-- ((==) e) x || any ((==) e) xs'
--               ----------------
-- =                                   HI
-- ((==) e) x || elem e xs'



-- elemB :: Eq a => a -> [a] -> Bool
-- elemB e []     = False
-- elemB e (x:xs) = e == x || elemB e xs

-- anyB :: (a -> Bool) -> [a] -> Bool
-- anyB f []     = False
-- anyB f (x:xs) = f x || anyB f xs

-- ---------------------------------------------------------------------------

-- -- d. 
-- para todo x. any (elem x) = elem x . concat
-- por ppio de extensionalidad
-- para todo x. para todo xss. any (elem x) xss = (elem x . concat) xss
-- por def de (.)
-- para todo x. para todo xss. any (elem x) xss = elem x (concat xss)
-- sea zss de tipo lista de lista finitas. por ppio de induccion sobre la estructura de zss , es equivalente demostrar que:

-- Caso Base: zss = []

-- any (elem x) [] = elem x (concat []) ?

-- Caso inductivo: zss = xs:xss'

-- HI) any (elem x) xss' = elem x (concat xss')!
-- TI) any (elem x) (xs:xss') = elem x (concat (xs:xss'))?


-- Caso Base:

-- LI)

-- any (elem x) []
-- ---------------
-- =                   def any
-- False

-- LD)

-- elem x (concat [])
--         ---------
-- =                   def concat
-- elem x []
-- ---------
-- =                   def elem
-- False

-- Caso Inductivo:

-- LI)

-- any (elem x) (xs:xss')
-- ----------------------
-- =                          def any
-- (elem x) xs || any (elem x) xss'
--        ----------
-- =                           HI
-- elem x xs || elem x (concat xss')
-- ---------------------------------
-- =                           LEMA propDistrideElem
-- elem x ( xs ++ concat xss')

-- LD)

-- elem x (concat (xs:xss'))
--         ----------------
-- =                           def concat
-- elem x (xs ++ concat xss')


-- LEMA: propDistrideElem
-- por ppio de extensionalidad
-- para todo xs. para todo ys. elem x xs || elem x ys = elem x (xs ++ ys)
-- sean ms y ns dos listas finitas, por ppio de induccion sobre la estructura de ms, es equivalente demostrar que:

-- Caso base: ms = []

-- elem x [] || elem x ns = elem x ([] ++ ns)?

-- Caso inductivo: ms = (m : ms')

-- HI) elem x ms' || elem x ns = elem x (ms ++ ns) !
-- TI) elem x (m: ms') || elem x ns = elem x ((m: ms') ++ ns) ?

-- Caso Base:

-- LI)

-- elem x [] || elem x ns
-- ---------
-- =                       def elem
-- False || elem x ns
-- ------------------
-- =                       def de neutro
-- elem x ns

-- LD)

-- elem x ([] ++ ns)
--         --------
-- =                       def ++
-- elem x ns

-- Caso Inductivo:

-- LI)

-- elem x (m: ms') || elem x ns 
-- ---------------
-- =                       def elem
-- x == m || elem x ms' || elem x ns
--         -------------------------
-- =                       HI
-- x == m || elem x (ms ++ ns)

-- LD)

-- elem x ((m: ms') ++ ns)
--         --------------
-- =                       def ++
-- elem x (m : ( ms' ++ ns))
-- -------------------------
-- =                       def elem
-- x == m || elem x (ms' ++ ns)


-- -- (++) :: [a] -> [a] -> [a]
-- -- (++) []     ys = ys
-- -- (++) xs     [] = xs
-- -- (++) (x:xs) ys = x : (xs (++) ys)

-- concat :: [[a]] -> [a]
-- concat []       = []
-- concat (xs:xss) = xs ++ concat xss

-- any :: (a -> Bool) -> [a] -> Bool
-- any f []     = False
-- any f (x:xs) = f x || any f xs

-- elem :: Eq a => a -> [a] -> Bool
-- elem e []     = False
-- elem e (x:xs) = e == x || elem e xs

-- ---------------------------------------------------------------------------

-- -- e. 
-- para todo xs. para todo ys. subset xs ys = all (flip elem ys) xs
-- sean zs y ws dos listas finitas, por ppio de induccion sobre la estructura de zs es equivalente demostrar que:

-- Caso Base: zs = []

-- subset [] ws = all (flip elem ws) [] ?

-- Caso Inductivo: zs = (x : xs')

-- HI) subset xs' ws = all (flip elem ws) xs' !
-- TI) subset (x:xs') ws = all (flip elem ws) (x:xs') ?

-- Caso Base

-- LI)

-- subset [] ws
-- ------------
-- =                       def subset
-- True

-- LD)

-- all (flip elem ws) []
-- ---------------------
-- =                       def all
-- True


-- Caso inductivo:

-- LI)

-- subset (x:xs') ws
-- -----------------
-- =                       def subset
-- (elem x ws) && subset xs' ws
--                 ------------
-- =                       HI
-- (elem x ws) && all (flip elem ws) xs'


-- LD)

-- all (flip elem ws) (x:xs')
-- --------------------------
-- =                       def all
-- (flip elem ws) x && all (flip elem ws) xs'
-- ----------------
-- =                       def flip
-- elem x ws && all (flip elem ws) xs'




-- subset :: Eq a => [a] -> [a] -> Bool
-- subset []     ys = True
-- subset (x:xs) ys = (elem x ys) && subset xs ys

-- all :: (a -> Bool) -> [a] -> Bool
-- all f []     = True
-- all f (x:xs) = f x && all f xs

-- flip' :: (b -> a -> c) -> a -> b -> c
-- flip' f x y = f y x 

-- ---------------------------------------------------------------------------

-- -- f. all null = null . concat
-- por ppio de extensionalidad
-- para todo xs. all null xs = (null . concat) xs
-- por def de (.)
-- para todo xs. all null xs = null (concat xs)
-- sea zs una lista de lista finita, por ppio de induccion sobre la estructura de zs, es equivalente demostrar que:

-- Caso Base: zs = []

-- all null [] = null (concat []) ?


-- Caso inductivo: zs = x : xs')

-- HI) all null xs' = null (concat xs') !
-- TI) all null (x:xs') = null (concat (x:xs')) ?


-- Caso Base:

-- LI)

-- all null []
-- -----------
-- =                       def all
-- True

-- LD)

-- null (concat [])
--      ----------
-- =                       def concat
-- null []
-- -------
-- =                       def null
-- True


-- Caso inductivo:

-- LI)

-- all null (x:xs')
-- ----------------
-- =                       def all
-- null x && all null xs'
--          ------------
-- =                       HI
-- null x && null (concat xs')
-- ----------------------------
-- =                       LEMA
-- null (x ++ concat xs')


-- LD)

-- null (concat (x:xs'))
--       --------------
-- =                       def concat
-- null (x ++ concat xs')


-- LEMA:
-- por ppio de extensionalidad
-- para todo k. para todo ks. null k && null ks = null (k ++ ks)
-- sean y un elem cualquiera y ys una lista cualquiera finita, por ppio de induccion sobre la estructura de ys, es equivalente demostrar que:

-- CASO base : ys = []

-- null y && null [] = null (y ++ [])

-- Caso inductivo : ys = k':ks')

-- HI) null y && null ks' = null (y ++ ks') !
-- TI) null y && null (k':ks') = null (y ++ (k':ks')) ?

-- Caso Base:

-- LI)

-- null y && null []
--           ------
-- =                   def null
-- null y && True
-- --------------
-- =                   neutro &&
-- null y

-- LD)

-- null (y ++ [])
--     ----------
-- =                   def ++
-- null y


-- Caso inductivo:

-- LI)

-- null y && null (k':ks')
--           -------------
-- =                       def null
-- null y && False
-- ---------------
-- =                       def &&
-- False

-- LD)

-- null (y ++ (k':ks'))
--       -------------
-- =                       def ++
-- null (y : (k ++ ys))
-- --------------------
-- =                       def null
-- False



-- concat :: [[a]] -> [a]
-- concat []       = []
-- concat (xs:xss) = xs ++ concat xss

-- all :: (a -> Bool) -> [a] -> Bool
-- all f []     = True
-- all f (x:xs) = f x && all f xs

-- -- que describe el resultado de agregar los elementos de la primera lista adelante de los elementos de la segunda.
-- (++) :: [a] -> [a] -> [a]
-- (++) []     ys = ys
-- (++) xs     [] = xs
-- (++) (x:xs) ys = x : (xs (++) ys)

-- ---------------------------------------------------------------------------

-- -- g. length = length . reverse
-- Prop: length = length . reverse
-- por ppio de extensionalidad
-- para todo xs. length xs = (length . reverse) xs
-- por def de (.)
-- para todo xs. length xs = length (reverse xs)
-- sea zs una lista de elementos finitos, por ppio de induccion sobre la estructura zs es equivalente demostrar que:

-- Caso Base: zs = []

-- length [] = length (reverse [])

-- Caso Inductivo: zs = (x: xs')

-- HI) length xs' = length (reverse xs') !
-- TI) length (x:xs') = length (reverse (x:xs')) ?

-- Caso Base

-- LI)

-- length []
-- ---------
-- =                       def length
-- 0

-- LD)

-- length (reverse [])
--         ----------
-- =                       def reverse
-- length []
-- ---------
-- =                       def length
-- 0

-- Caso Inductivo

-- LI)

-- length (x: xs')
-- ---------------
-- =                       def length
-- 1 + length xs'
--     ----------
-- =                       HI
-- 1 + length (reverse xs')


-- LD)

-- length (reverse (x:xs'))
--         ---------------
-- =                       def reverse
-- length ((reverse xs') ++ [x])
-- -----------------------------
-- =                       LEMA
-- length (reverse xs') + length (x: [])
--                         -------------
-- =                       def length
-- length (reverse xs') + 1 + length []
--                             --------
-- =                       def length
-- length (reverse xs') + 1 + 0
-- ----------------------------
-- =                       aritm
-- 1 + length (reverse xs')


-- Lema: 
-- por ppio de extensionalidad
-- para todo xs. para todo ys. length ( xs ++ ys) = length xs + length ys
-- sea ts y ps dos listas finitas, por ppio de induccion sobre la estructura de ts es equivalente demostrar que:

-- Caso Base: ts = []

-- length ([] ++ ps) = length [] + length ps ?

-- Caso Inductivo: ts = x : xs')

-- HI) length (xs' ++ ps) = length xs' + length ps !
-- TI) length ((x: xs') ++ ps) = length (x: xs') + length ps ?

-- Caso base:

-- LI)

-- length ([] ++ ps)
--         --------
-- =                       def ++
-- length ps

-- LD)

-- length [] + length ps
-- ---------
-- =                       def length
-- 0 + length ps
-- -------------
-- =                       neutro de la suma
-- length ps


-- Caso inductivo:

-- LI)

-- length ((x: xs') ++ ps)
--         ---------------
-- =                           def ++
-- length (x : (xs' ++ ps))
-- ------------------------
-- =                           def length
-- 1 + length (xs' ++ ps)
--     ------------------
-- =                           HI
-- 1 + length xs' + length ps


-- LD)

-- length (x: xs') + length ps
-- ---------------
-- =                           def length
-- 1 + length xs' + length ps


-- verse :: [a] -> [a]
-- verse []     = []
-- verse (x:xs) = (verse xs) ++ [x]

-- -- (++) :: [a] -> [a] -> [a]
-- -- (++) []     ys = ys
-- -- (++) xs     [] = xs
-- -- (++) (x:xs) ys = x : (xs (++) ys)

-- h. para todo xs. para todo ys.
-- reverse (xs ++ ys) = reverse ys ++ reverse xs
-- sean zs, ws dos listas finitas, por ppio de induccion sobre la estructura de zs, es equivalente demostrar que:

-- Caso Base: zs = []

-- reverse ([] ++ ws) = reverse ws ++ reverse [] ?

-- Caso Inductivo: zs = x:xs')

-- HI) reverse (xs' ++ ws) = reverse ws ++ reverse xs' !
-- TI) reverse ((x:xs') ++ ws) = reverse ws ++ reverse (x:xs') ?

-- Caso Base: 

-- LI)

-- reverse ([] ++ ws)
--         ----------
-- =                   def ++
-- reverse ws

-- LD)

-- reverse ws ++ reverse []
--               ----------
-- =                   def reverse
-- reverse ws ++ []
-- ----------------
-- =                   def ++
-- reverse ws

-- Caso inductivo:

-- LI)

-- reverse ((x:xs') ++ ws)
--         --------------
-- =                       def ++
-- reverse( x : (xs' ++ ws))
-- -------------------------
-- =                       def (reverse
-- reverse (xs' ++ ws) ++ [x]

-- LD)

-- reverse ws ++ reverse (x:xs')
--               ---------------
-- =                       def reverse
-- reverse ws ++ (reverse xs') ++ [x]
-- ---------------------------
-- =                       HI
-- reverse (xs' ++ ws) ++ [x]

-- -- (++) :: [a] -> [a] -> [a]
-- -- (++) []     ys = ys
-- -- (++) xs     [] = xs
-- -- (++) (x:xs) ys = x : (xs (++) ys)

-- -- verse :: [a] -> [a]
-- -- verse []     = []
-- -- verse (x:xs) = (verse xs) ++ [x]


-- -- i. 
-- para todo xs. para todo ys. all p (xs++ys) = all p (reverse xs) && all p (reverse ys) ?
-- sea zs y ws dos listas finitas. Por ppio de induccion sobre la estructura de zs, es equivalente demostrar que:

-- Caso Base: zs = []

-- all p ([] ++ ys) = all p (reverse []) && all p (reverse ws) ?

-- Caso inductivo: zs = x:xs')

-- HI) all p (xs'++ ws) = all p (reverse xs') && all p (reverse ws) !
-- TI) all p ((x:xs') ++ ws) = all p (reverse (x:xs')) && all p (reverse ws) ?

-- Caso Base:

-- LI)

-- all p ([] ++ ws)
--         -------
-- =                       def ++
-- all p ws
-- --------
-- =                       LEMA 1:
-- all p (reverse ws)


-- LD)

-- all p (reverse []) && all p (reverse ws)
--         ---------
-- =                       def reverse
-- all p [] && all p (reverse ws)
-- --------
-- =                       def all
-- True && all p (reverse ws)
-- -------------------------
-- =                       neutro &&
-- all p (reverse ws)

-- ---------------------------------------------------------------------------------------

-- Caso inductivo:

-- LI) 

-- all p ((x:xs') ++ ws)
--         ------------
-- =                       def ++
-- all p (x : (xs' ++ ws))
-- -----------------------
-- =                       def all
-- p x && all p (xs' ++ ws)
--         ----------------
-- =                       HI
-- p x && all p (reverse xs') && all p (reverse ws)


-- LD)

-- all p (reverse (x:xs')) && all p (reverse ws)
--         ---------------
-- =                       def reverse
-- all p (reverse xs' ++ [x]) && all p (reverse ws)
-- -------------------------
-- =                       LEMA 2: distrubutiva
-- all p (reverse xs') && all p [x] && all p (reverse ws)
--                         -------
-- =                       def all
-- all p (reverse xs') && p x && all p [] && all p (reverse ws)
--                               --------
-- =                       def all
-- all p (reverse xs') && p x && True && all p (reverse ws)
--                                 ------------------------
-- =                       && es neutro 
-- all p (reverse xs') && p x && all p (reverse ws)
-- --------------------------
-- =                       conmutatividad
-- p x && all p (reverse xs') && all p (reverse ws)


-- all :: (a -> Bool) -> [a] -> Bool
-- all f []     = True
-- all f (x:xs) = f x && all f xs

-- reverse :: [a] -> [a]
-- reverse []     = []
-- reverse (x:xs) = (reverse xs) ++ [x]

-- -- (++) :: [a] -> [a] -> [a]
-- -- (++) []     ys = ys
-- -- (++) xs     [] = xs
-- -- (++) (x:xs) ys = x : (xs (++) ys)


-- ---------------------------------------------------------------------------------------

-- -- ## Definicion del primer LEMA ##

-- LEMA 1: allP
-- por ppio de extensionalidad.
-- para todo xs. all p xs = all p (reverse xs) ?
-- sea bs una lista finita, por ppio de induccion sobre la estructura de bs, es equivalente demostrar que:

-- Caso Base: bs = []

-- all p [] = all p (reverse [])?

-- Caso inductivo: bs = x:xs')

-- HI) all p xs' = all p (reverse xs') !
-- TI) all p (x:xs') = all p (reverse (x:xs')) ?


-- Caso Base:

-- LI)

-- all p []
-- --------
-- =                       def all
-- True

-- LD)

-- all p (reverse [])
--         --------
-- =                       def reverse
-- all p []
-- --------
-- =                       def all
-- True


-- Caso inductivo:

-- LI)

-- all p (x:xs')
-- -------------
-- =                       def all
-- p x && all p xs'
--         -------
-- =                       HI
-- p x && all p (reverse xs')


-- LD)

-- all p (reverse (x:xs'))
--         -------------
-- =                       def reverse
-- all p (reverse xs' ++ [x])
-- --------------------------
-- =                       LEMA 2: distrubutiva
-- all p (reverse xs') && all p [x]
--                         --------
-- =                       def all
-- all p (reverse xs') && p x && all f []
--                               --------
-- =                       def all
-- all p (reverse xs') && p x && True
--                         ----------
-- =                       && neutro
-- all p (reverse xs') && p x
-- --------------------------
-- =                       conmutatividad &&
-- p x && all p (reverse xs')


-- all :: (a -> Bool) -> [a] -> Bool
-- all f []     = True
-- all f (x:xs) = f x && all f xs

-- ---------------------------------------------------------------------------------------

-- -- ## Definicion del primer LEMA ##

-- LEMA 2: distrubutiva
-- por ppio de extensionalidad.
-- para todo xs. para todo ys. all p (xs ++ ys) = all p xs && all p ys ?
-- sea zs y ws dos listas finitas, por ppio de induccion sobre la estructura de zs, es equivalente demostrar que:

-- Caso Base: zs = [] 

-- all p ([] ++ ws) = all p [] && all p ws?

-- Caso inductivo: zs = x : xs')

-- HI) all p (xs' ++ ws) = all p xs' && all p ws !
-- TI) all p ((x: xs') ++ ws) = all p (x: xs') && all p ws ?

-- Caso Base

-- LI)

-- all p ([] ++ ws)
--         --------
-- =                       def ++
-- all p ws

-- LD)

-- all p [] && all p ws
-- --------
-- =                       de all
-- True && all p ws
-- ----------------
-- =                       neutro &&
-- all p ws


-- Caso inductivo:

-- LI)

-- all p ((x: xs') ++ ws)
--         -------------
-- =                       def ++
-- all p (x : (xs' ++ ws))
-- -----------------------
-- =                       def all
-- p x && all p (xs' ++ ws)


-- LD)

-- all p (x: xs') && all p ws
-- --------------
-- =                       def all
-- p x && all p xs' && all p ws
--         --------------------
-- =                       HI
-- p x && all p (xs' ++ ws)

---------------------------------------------------------------------------------------

-- j. para todo xs. para todo ys. unzip (zip xs ys) = (xs, ys)
-- (en este caso, mostrar que no vale)

------------------------------------ ### SECCION 2 ### ------------------------------------

-- f Z     = ...
-- f (S n) = ... f n

cinco = S (S (S (S (S Z))))

tres = S (S (S Z))

data N = Z | S N deriving (Show)

evalN :: N -> Int
evalN Z     = 0
evalN (S n) = 1 + evalN n

addN :: N -> N -> N
addN Z m     = m
addN (S n) m = S (addN n m)

prodN :: N -> N -> N
prodN Z m     = Z
prodN (S n) m = addN m (prodN n m)

int2N :: Int -> N
int2N 0 = Z
int2N n = S (int2N (n - 1))


-- ### DEMOS ###

-- i.
-- para todo n1. para todo n2. evalN (addN n1 n2) = evalN n1 + evalN n2
-- sean m1 y m2 dos elementos de tipo N. Por ppio de induccion sobre la estructura de m1, es equivalente demostrar que:

-- CASO BASE: m1 = Z

-- evalN (addN Z m2) = evalN Z + evalN m2 ?

-- CASO Inductivo : m1 = S n)

-- HI) evalN (addN n m2) = evalN n + evalN m2 !
-- TI) evalN (addN (S n) m2) = evalN (S n) + evalN m2 ?

-- CASO BASE:

-- LI)

-- evalN (addN Z m2)
--         --------
-- =                       def addN
-- evalN m2

-- LD)

-- evalN Z + evalN m2
-- -------
-- =                       def evalN
-- 0 + evalN m2
-- ------------
-- =                       neutro suma
-- evalN m2


-- Caso Inductivo:

-- LI)

-- evalN (addN (S n) m2)
--         ------------
-- =                       def addN
-- evalN (S (addN n m2))
-- ---------------------
-- =                       def evalN
-- 1 + evalN (addN n m2)
--     -----------------
-- =                       HI
-- 1 + evalN n + evalN m2

-- LD)

-- evalN (S n) + evalN m2
-- -----------
-- =                       def evalN
-- 1 + evalN n + evalN m2

-- ---------------------------------------------------------------------------------------


-- -- ii.
-- para todo n1. para todo n2. evalN (prodN n1 n2) = evalN n1 * evalN n2
-- sean m1 y m2 dos elementos de tipo N. Por ppio de induccion sobre la estructura de m1, es equivalente demostrar que:

-- CASO BASE: m1 = Z

-- evalN (prodN Z m2) = evalN Z * evalN m2? 

-- CASO Inductivo: m1 = (S n)

-- HI) evalN (prodN n m2) = evalN n * evalN m2 !
-- TI) evalN (prodN (S n) m2) = evalN (S n) * evalN m2 ?

-- CASO BASE

-- LI)

-- evalN (prodN Z m2) 
--         ---------
-- =                       def prodN
-- evalN Z
-- -------
-- =                       def evalN
-- 0

-- LD)

-- evalN Z * evalN m2
-- -------
-- =                       def evalN
-- 0 * evalN m2
-- -----------
-- =                       prop * cero
-- 0

-- CASO Inductivo

-- LI)

-- evalN (prodN (S n) m2)
--         -------------
-- =                       def prodN
-- evalN (addN m2 (prodN n m2))
-- ----------------------------
-- =                       prop ya demostrada en ejercicio anterior
-- evalN m2 + evalN (prodN n m2)


-- LD)

-- evalN (S n) * evalN m2
-- -----------
-- =                       def evalN
-- (1 + evalN n) * evalN m2
-- -------------------------
-- =                       distrubutiva ?
-- evalN m2 + evalN n * evalN m2
--           -------------------
-- =                       HI
-- evalN m2 + evalN (prodN n m2)



-- iii. int2N . evalN = id ?
-- por ppio de extensionalidad.
-- para todo n. (int2N . evalN) n = id n ?
-- por def de (.)
-- para todo n. int2N (evalN n) = id n ?
-- por def de id
-- para todo n. int2N (evalN n) = n?
-- sea m de tipo N, por ppio de induccion sobre la estructura de m, es equivalente demostrar que:

-- CASO BASE: m = Z

-- int2N (evalN Z) = Z ?

-- CASO INDUCTIVO: m = S n')

-- HI) int2N (evalN n') = n' !
-- TI) int2N (evalN (S n')) = (S n') ?

-- CASO BASE:

-- LI)

-- int2N (evalN Z)
--         -------
-- =                       def evalN
-- int2N 0
-- -------
-- =                       def int2N
-- Z

-- LD)

-- Z

-- CASO INDUCTIVO:

-- LI)

-- int2N (evalN (S n'))
--         -----------
-- =                       def evalN
-- int2N (1 + evalN n')
-- --------------------
-- =                       def int2N
-- S (int2N (1 + evalN n' 1) - 1)
--         ---------------------
-- =                       cancelo los 1-1
-- S (int2N (evalN n'))
-- --------------------
-- =                       HI
-- (S n')

-- LD)

-- (S n')


data Unit = Unit

cuatroElem = [(), (), (), ()]

type NU = [()]

evalNU :: NU -> Int
evalNU []     = 0
evalNU (n:ns) = 1 + evalNU ns

succNU :: NU -> NU
succNU [] = [()]
succNU ns = () : succNU ns

addNU :: NU -> NU -> NU
addNU [] m     = m
addNU (n:ns) m = n : (addNU ns m)

nu2n :: NU -> N
nu2n []     = Z
nu2n (n:ns) = S (nu2n ns)

n2nu :: N -> NU
n2nu Z     = []
n2nu (S n) = () : n2nu n

------------------------------------------ ### DEMOS ### ------------------------------------------

-- i. evalNU . succNU = (+1) . evalNU ?
-- por ppio de extensionalidad.
-- para todo n. (evalNU . succNU) n = ((+1) . evalNU) n ?
-- por def de (.) Dos veces.
-- para todo n. evalNU (succNU n) = (+1) (evalNU n) ?
-- sea nu de tipo NU. Por ppio de induccion sobre la estructura de nu es equivalente demostrar que:

-- CASO BASE: nu = []

-- evalNU (succNU []) = (+1) (evalNU []) ?

-- CASO INDUCTIVO: nu = (n:ns')

-- HI) evalNU (succNU ns') = (+1) (evalNU ns') !
-- TI) evalNU (succNU (n:ns')) = (+1) (evalNU (n:ns')) ?


-- CASO BASE:

-- LI)

-- evalNU (succNU [])
--         ---------
-- =                       def succNU
-- evalNU [()]
-- -----------
-- =                       def evalNU
-- 1 + evalNU []
--     ---------
-- =                       def evalNU
-- 1 + 0
-- -----
-- =                       def +
-- 1

-- LD)

-- (+1) (evalNU [])
--       ---------
-- =                       def evalNU
-- (+1) 0
-- ------
-- =                       def +
-- 1

-- CASO INDUCTIVO:

-- LI)

-- evalNU (succNU (n:ns'))
--         --------------
-- =                       def succNU
-- evalNU (() : succNU ns')
-- ------------------------
-- =                       def evalNU
-- 1 + (+1) (evalNU ns')
-- --------
-- =                       def de +
-- 2 + (evalNU ns')

-- LD)

-- (+1) (evalNU (n:ns'))
--       --------------
-- =                       def evalNU
-- (+1) (1 + evalNU ns')
-- -------
-- =                       def de +
-- 2 + evalNU ns'


-- ii. para todo n1. para todo n2.
-- evalNU (addNU n1 n2) = evalNU n1 + evalNU n2
-- sea nu1 y nu2 dos elementos de tipo NU. Por ppio de induccion sobre la estructura de nu1, es equivalente demostrar que:

-- CASO BASE: nu1 = []

-- evalNU (addNU [] nu2) = evalNU [] + evalNU nu2 ?


-- CASO INDUCTIVO: nu1 = (n:ns)

-- HI) evalNU (addNU ns nu2) = evalNU ns + evalNU nu2 !
-- TI) evalNU (addNU (n:ns) nu2) = evalNU (n:ns) + evalNU nu2 ?

-- CASO BASE:

-- LI)

-- evalNU (addNU [] nu2)
--         ------------
-- =                       def addNU
-- evalNU nu2

-- LD)

-- evalNU [] + evalNU nu2
-- ---------
-- =                       def evalNU
-- 0 + evalNU nu2
-- --------------
-- =                       0 es neutro
-- evalNU nu2

-- CASO INDUCTIVO:

-- LI)

-- evalNU (addNU (n:ns) nu2)
--         ----------------
-- =                       def addNU
-- evalNU (n : (addNU ns nu2))
-- --------------------------
-- =                       def evalNU
-- 1 + evalNU (addNU ns nu2)

-- LD)

-- evalNU (n:ns) + evalNU nu2
-- ------------
-- =                       def evalNU
-- 1 + evalN ns + evalNU nu2
--     ---------------------
-- =                       HI
-- 1 + evalNU (addNU ns nu2)




-- iii. nu2n . n2nu = id
-- por ppio de extensionalidad
-- para todo n. (nu2n . n2nu) n = id n?
-- por def de (.)
-- para todo n. nu2n (n2nu n) = id n ?
-- por def de id
-- para todo n. nu2n (n2nu n) = n ?
-- sea nu de tipo N. por ppio de induccion sobre la estructura de nu, es equivalente demostrar que:

-- CASO BASE: nu = Z

-- nu2n (n2nu Z) = Z ?

-- CASO INDUCTIVO: nu = (S n')

-- HI) nu2n (n2nu n') = n' !
-- TI) nu2n (n2nu (S n')) = (S n') ?

-- CASO BASE:

-- LI)

-- nu2n (n2nu Z)
--      --------
-- =               def n2nu
-- nu2n []
-- -------
-- =               def nu2n
-- Z

-- LD)

-- Z

-- CASO INDUCTIVO:

-- LI)

-- nu2n (n2nu (S n'))
--       ----------
-- =               def n2nu
-- nu2n (() : n2nu n')
-- -------------------
-- =               def nu2n
-- S ( nu2n (n2nu n'))
--   -----------------
-- =               HI
-- S n'

-- LD)

-- S n'

---------------------------- ## EJERCICIO 3 ### ----------------------------

type NBin = [DigBin]

data DigBin = O | I deriving (Show)

-- f [] = ...
-- f (db:dbs) = op db (f dbs)

-- op puede chequear casos sobre db y sobre lo que vuelve de la recursión

-- a.dado un símbolo que representa un dígito binario lo transforma en su significado como número
dbAsInt :: DigBin -> Int
dbAsInt O = 0
dbAsInt I = 1

-- b. que dado un símbolo que representa un dígito binario lo transforma en su significado como booleano.
dbAsBool :: DigBin -> Bool
dbAsBool I = True
dbAsBool O = False

-- c. que dado un booleano lo transforma en el símbolo que representa a ese booleano.
dbOfBool :: Bool -> DigBin
dbOfBool True = I
dbOfBool False = O

-- d. que dado un dígito binario lo transforma en el otro.
negDB :: DigBin -> DigBin
negDB I = O
negDB O = I

succNB :: NBin -> NBin
succNB []      = [I]
succNB (O:dbs) = I : dbs
succNB (I:dbs) = O : (succNB dbs)

addNB :: NBin -> NBin -> NBin
addNB [] nbin  = nbin
addNB nbin []  = nbin
addNB (db:dbs) (db':dbs') = addDB db db' (addNB dbs dbs')

addDB I I dbs = O : succNB dbs
addDB O O dbs = O : dbs
addDB I O dbs = I : dbs
addDB O I dbs = I : dbs

normalizarNB :: NBin -> NBin
normalizarNB []       = []
normalizarNB (db:dbs) = normalizar db (normalizarNB dbs)

normalizar :: DigBin -> NBin -> NBin
normalizar O [] = []
normalizar d ds = d : ds

n2nb :: N -> NBin
n2nb Z     = []
n2nb (S n) = succNB (n2nb n)

evalNB :: NBin -> Int 
evalNB []      = 0
evalNB (d:dbs) = dbAsInt d + (2 * evalNB dbs)

nb2n :: NBin -> N
nb2n []      = Z
nb2n (d:dbs) = addN (evalD d) (prodN (S (S Z)) (nb2n dbs))

evalD O = Z
evalD I = (S Z)

-------------------------------------- ## DEMOS ## --------------------------------------

-- i)
-- evalNB . normalizarNB = evalNB ?

-- por ppio de extensionalidad
-- para todo nb. (evalNB . normalizarNB) nb = evalNB nb ?
-- por def de (.)
-- para todo nb. evalNB (normalizarNB nb) = evalNB nb ?
-- sea nbin de tipo NBin, por ppio de induccion sobre la estructura de nbin, es equivalente demostrar que:

-- CASO BASE: nbin = []

-- evalNB (normalizarNB []) = evalNB [] ?

-- CASO INDUCTIVO: nbin = (n:ns)

-- SUBCASO n = O 

-- HI) evalNB (normalizarNB ns) = evalNB ns !
-- TI) evalNB (normalizarNB (O:ns)) = evalNB (O:ns) ?

-- SUBCASO n = I

-- HI) evalNB (normalizarNB ns) = evalNB ns !
-- TI) evalNB (normalizarNB (I:ns)) = evalNB (I:ns) ?


-- CASO BASE:

-- LI)

-- evalNB (normalizarNB [])
--         ---------------
-- =               def normalizarNB
-- evalNB []
-- ---------
-- =               def evalNB
-- 0

-- LD)

-- evalNB []
-- ---------
-- =               def evalNB
-- 0

-- CASO INDUCTIVO:

-- Subcaso = O 

-- LI)

-- evalNB (normalizarNB (O:ns))
--         -------------------
-- =               def normalizarNB
-- evalNB (normalizar O (normalizarNB ns))
-- ---------------------------------------
-- =               LEMA evalNB 2*
-- 2 * (evalNB (normalizarNB ns))


-- LD)

-- evalNB (O:ns)
-- -------------
-- =               def evalNB
-- dbAsInt O + (2 * evalNB ns)
--                 ----------
-- =               HI
-- dbAsInt O + (2 * (evalNB (normalizarNB ns)))
-- ---------
-- =               def dbAsInt
-- 0 + (2 * (evalNB (normalizarNB ns)))
-- ----------------------------------
-- =               def suma
-- 2 * (evalNB (normalizarNB ns))


-- CASO INDUCTIVO:

-- Subcaso = I

-- LI)

-- evalNB (normalizarNB (I:ns)) 
--         -------------------
-- =               def normalizarNB
-- evalNB (normalizar I (normalizarNB ns))
--         ------------------------------
-- =               def normalizar
-- evalNB (I : (normalizar ns))
-- ----------------------------
-- =               def evalNB
-- dbAsInt I + (2 * evalNB(normalizar ns))
--                 ----------------------
-- =               HI
-- dbAsInt I + (2 * evalNB ns)
-- ---------
-- =               def dbAsInt
-- 1 + (2 * evalNB ns)

-- LD)

-- evalNB (I:ns)
-- -------------
-- =               def evalNB
-- dbAsInt I + (2 * evalNB ns)
-- ---------
-- =               def dbAsInt
-- 1 + (2 * evalNB ns)


-- -- ### LEMA evalNB 2* ###
-- por ppio de extensionalidad
-- para todo xs. evalNB (normalizar O xs) = 2 * (evalNB xs)
-- voy a demostrar por casos sobre xs:

-- Caso xs= []

-- LI)

-- evalNB (normalizar O [])
--         ---------------
-- =                       def normalizar
-- evalNB []
-- ---------
-- 0

-- LD)

-- 2 * (evalNB [])
--     ----------
-- =                       def evalNB
-- 2 * 0
-- -----
-- =                       def *
-- 0

-- Caso Inductivo xs = x:xs')

-- LI)

-- evalNB (normalizar O (x:xs'))
--         --------------------
-- =                       def normalizar
-- evalNB (O : (x:xs'))
-- --------------------
-- =                       def evalNB
-- dbAsInt O + (2 * (evalNB (x:xs')))
-- ---------
-- =                       def dbAsInt
-- 0 + 2 * (evalNB (x:xs'))
-- -------------------------
-- =                       def +
-- 2 * (evalNB (x:xs'))


-- LD)

-- 2 * (evalNB (x:xs'))

-- ii)
-- evalNB . succNB = (+1) . evalNB ?
-- por ppio de extensionalidad
-- para todo nb. (evalNB . succNB) nb = ((+1) . evalNB) nb ?
-- por def de (.) dos veces
-- para todo nb. evalNB (succNB nb) = (+1) (evalNB nb) ?
-- sea nbin de tipo NBin. Por ppio de induccion sobre la estructura de nbin es equivalente demostrar que:

-- CASO BASE: nbin = []

-- evalNB (succNB []) = (+1) (evalNB []) ?

-- CASO INDUCTIVO: nbin = n: ns)

-- Subcaso nbin = O

-- HI) evalNB (succNB ns) = (+1) (evalNB ns) !
-- TI) evalNB (succNB (O: ns)) = (+1) (evalNB (O: ns)) ?

-- Subcaso nbin = I

-- HI) evalNB (succNB ns) = (+1) (evalNB ns) !
-- TI) evalNB (succNB (I: ns)) = (+1) (evalNB (I: ns)) ?



-- CASO BASE:

-- LI)

-- evalNB (succNB [])
--         ---------
-- =               def succNB
-- evalNB [I]
-- ----------
-- =               def evalNB
-- dbAsInt I + (2 * evalNB [])
-- ---------
-- =               def dbAsInt
-- 1 + (2 * evalNB [])
--         ----------
-- =               def evalNB
-- 1 + (2 * 0)
--     -------
-- =               def *
-- 1

-- LD)

-- (+1) (evalNB [])
--       ---------
-- =               def evalNB
-- (+1) 0
-- ------
-- =               def +
-- 1


-- CASO INDUCTIVO:

-- Caso n = O

-- LI)

-- evalNB (succNB (O: ns))
--         --------------
-- =               def succNB
-- evalNB (I : ns)
-- ---------------
-- =               def evalNB
-- dbAsInt I + (2 * evalNB ns)
-- ---------
-- =               def dbAsInt
-- 1 + (2 * evalNB ns)
-- -------------------
-- =               def +
-- (2 * evalNB ns)

-- LD)

-- (+1) (evalNB (O: ns))
--       --------------
-- =               def evalNB
-- (+1) (dbAsInt O + (2 * evalNB ns))
--       ---------
-- =               def dbAsInt
-- (+1) (0 + (2 * evalNB ns))
--       -------------------
-- =               0 es neutro
-- (+1) (2 * evalNB ns)
-- --------------------
-- =               def +
-- (2 * evalNB ns)


-- Caso n = I

-- LI)

-- evalNB (succNB (I: ns))
--         --------------
-- =               def succNB
-- evalNB (O : (succNB ns))
-- ------------------------
-- =               def evalNB
-- dbAsInt O + (2 * evalNB(succNB ns))
-- ---------
-- =               def dbAsInt
-- 0 + (2 * evalNB(succNB ns))
-- --------------------------
-- =               def +
-- (2 * evalNB(succNB ns))
--      -----------------
-- =               HI
-- (2 * (+1) (evalNB ns))


-- LD)

-- (+1) (evalNB (I: ns))
--       --------------
-- =               def evalNB
-- (+1) (dbAsInt I + (2 * evalNB ns))
--         --------
-- =               def dbAsInt
-- (+1) 1 + (2 * evalNB ns)
-- ------
-- =               def +
-- 2 + (2 * evalNB ns)
-- ------------------
-- =               factor comun
-- 2 * (1 + evalNB ns)

-- Subcaso nbin = I

-- HI) evalNB (succNB ns) = (+1) (evalNB ns) !
-- TI) evalNB (succNB (I: ns)) = (+1) (evalNB (I: ns)) ?

-- evalNB :: NBin -> Int 
-- evalNB []      = 0
-- evalNB (d:dbs) = dbAsInt d + (2 * evalNB dbs)

-- succNB :: NBin -> NBin
-- succNB []      = [I]
-- succNB (O:dbs) = I : dbs
-- succNB (I:dbs) = O : (succNB dbs)

-- iii) para todo n1. para todo n2. evalNB (addNB n1 n2) = evalNB n1 + evalNB n2?
-- sea nbin1 y nbin2 de tipo NBin. Por ppio de induccion sobre la estructura de nbin1 es esquivalente demostrar que:

-- CASO BASE: nbin1 = []

-- evalNB (addNB [] nbin2) = evalNB [] + evalNB nbin2 ?

-- CASO INDUCTIVO: nbin1 = n:ns)

-- HI) evalNB (addNB ns n2) = evalNB ns + evalNB n2 !
-- TI) evalNB (addNB (n:ns) n2) = evalNB (n:ns) + evalNB n2?


-- CASO BASE:

-- LI)

-- evalNB (addNB [] nbin2)
--         ---------------
-- =                       def addNB
-- evalNB nbin2


-- LD)

-- evalNB [] + evalNB nbin2
-- ---------
-- =                       def evalNB
-- 0 + evalNB nbin2
-- ----------------
-- =                       cero es neutro en suma
-- evalNB nbin2


-- CASO INDUCTIVO:

-- LI)

-- evalNB (addNB (n:ns) n2)
--         ---------------
-- =                       def addNB
-- evalNB ()
-- LD)

-- evalNB (n:ns) + evalNB n2 TERMINAR!


data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA deriving (Show)

expaSuma = Suma (Cte 10) (Cte 2)
expaProd = Prod (Cte 10) (Cte 3)
expaCero = Suma (Cte 0) (Cte 20)

evalExpA :: ExpA -> Int
evalExpA (Cte n)            = n
evalExpA (Suma expa1 expa2) = evalExpA expa1 + evalExpA expa2
evalExpA (Prod expa1 expa2) = evalExpA expa1 * evalExpA expa2

simplificarExpA :: ExpA -> ExpA
simplificarExpA (Cte n)            = (Cte n)
simplificarExpA (Suma expa1 expa2) = simplificarSuma (simplificarExpA expa1) (simplificarExpA expa2)
simplificarExpA (Prod expa1 expa2) = simplificarProd (simplificarExpA expa1) (simplificarExpA expa2)

simplificarSuma :: ExpA -> ExpA -> ExpA
simplificarSuma (Cte 0) exp     = exp
simplificarSuma exp (Cte 0)     = exp
simplificarSuma expa1 expa2     = Suma expa1 expa2

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd (Cte 0) _       = Cte 0
simplificarProd _ (Cte 0)       = Cte 0
simplificarProd exp (Cte 1)     = exp
simplificarProd (Cte 1) exp     = exp
simplificarProd expa1 expa2     = Prod expa1 expa2

cantidadDeSumaCero :: ExpA -> Int
cantidadDeSumaCero (Cte n)            = 0
cantidadDeSumaCero (Suma expa1 expa2) = sumarSiEsSumaCero expa1 expa2 + cantidadDeSumaCero expa1 + cantidadDeSumaCero expa2
cantidadDeSumaCero (Prod expa1 expa2) = cantidadDeSumaCero expa1 + cantidadDeSumaCero expa2

sumarSiEsSumaCero :: ExpA -> ExpA -> Int
sumarSiEsSumaCero (Cte 0) (Cte n) = 1
sumarSiEsSumaCero (Cte n) (Cte 0) = 1
sumarSiEsSumaCero _ _             = 0


ejemploLoco = Suma (Prod (Cte 1) (Cte 2)) (Prod (Cte 2) (Cte 0))

ejemploLoco2 = Suma (Cte 0) (Cte 0)

-- 1) evalExpA . simplificarExpA = evalExpA ?
-- por ppio de extensionalidad
-- para todo expa. (evalExpA . simplificarExpA) expa = evalExpA expa ?
-- por def de (.)
-- para todo expa. evalExpA (simplificarExpA expa) = evalExpA expa ?
-- sea exp de tipo ExpA. por ppio de induccion sobre la estructura de exp, es equivalente demostrar que:

-- CASO BASE: exp = (Cte n)

-- evalExpA (simplificarExpA (Cte n)) = evalExpA (Cte n) ?

-- CASO INDUCTIVO 1: exp = (Suma expa1 expa2)

-- HI1) evalExpA (simplificarExpA expa1) = evalExpA expa1 !
-- HI2) evalExpA (simplificarExpA expa2) = evalExpA expa2 !
-- TI) evalExpA (simplificarExpA (Suma expa1 expa2)) = evalExpA (Suma expa1 expa2) ?

-- CASO INDUCTIVO 2: exp = (Prod expa1 expa2)

-- HI1) evalExpA (simplificarExpA expa1) = evalExpA expa1 !
-- HI2) evalExpA (simplificarExpA expa2) = evalExpA expa2 !
-- TI) evalExpA (simplificarExpA (Prod expa1 expa2)) = evalExpA (Prod expa1 expa2) ?

-- CASO BASE:

-- LI)

-- evalExpA (simplificarExpA (Cte n))
--           -----------------------
-- =                                       def simplificarExpA
-- evalExpA (Cte n)

-- LD)

-- evalExpA (Cte n)


-- CASO INDUCTIVO 1:

-- LI)

-- evalExpA (simplificarExpA (Suma expa1 expa2))
--           ----------------------------------
-- =                                       def simplificarExpA
-- evalExpA (simplificarSuma (simplificarExpA expa1) (simplificarExpA expa2))
-- --------------------------------------------------------------------------
-- =                                       lema eval distributivo
-- evalExpA (simplificarExpA expa1) + evalExpA (simplificarExpA expa2)


-- LD)

-- evalExpA (Suma expa1 expa2)
-- ---------------------------
-- =                                       def evalExpA
-- evalExpA expa1 + evalExpA expa2
-- --------------   --------------
-- =                                       HI1, HI2
-- evalExpA (simplificarExpA expa1) + evalExpA (simplificarExpA expa2)


-- CASO INDUCTIVO 2:

-- LI)

-- evalExpA (simplificarExpA (Prod expa1 expa2)) 
--         ------------------------------------
-- =                                       def simplificarExpA
-- evalExpA (simplificarProd (simplificarExpA expa1) (simplificarExpA expa2))
-- --------------------------------------------------------------------------
-- =                                       lema distributivo-multi-PROD
-- evalExpA (simplificarExpA expa1) * evalExpA (simplificarExpA expa2)

-- LD)

-- evalExpA (Prod expa1 expa2)
-- ---------------------------
-- =                                       def evalExpA
-- evalExpA expa1 * evalExpA expa2
-- ---------------  ---------------
-- =                                       HI1, HI2
-- evalExpA (simplificarExpA expa1) * evalExpA (simplificarExpA expa2)


-- LEMA eval-distributivo-SUMA

-- por ppio de extensionalidad
-- para todo expa1. para todo expa2. evalExpA (simplificarSuma expa1 expa2) = evalExpA expa1 + evalExpA expa2 ?
-- voy a demostrar por casos:

-- CASO1 e1 = (Cte 0)

-- evalExpA (simplificarSuma (Cte 0) e2) = evalExpA (Cte 0) + evalExpA e2 ?

-- LI)

-- evalExpA (simplificarSuma (Cte 0) e2)
--          ---------------------------
-- =                                       def evalExpA
-- evalExpA e2

-- LD)

-- evalExpA (Cte 0) + evalExpA e2
-- ----------------
-- =                                       def evalExpA
-- 0 + evalExpA e2
-- ---------------
-- =                                       cero es neutro suma
-- evalExpA e2

-- CASO2: e2 = (Cte 0)

-- evalExpA (simplificarSuma e1 (Cte 0)) = evalExpA e1 + evalExpA (Cte 0) ?

-- LI)

-- evalExpA (simplificarSuma e1 (Cte 0))
--          ---------------------------
-- =                                       def simplificarSuma
-- evalExpA e1

-- LD)

-- evalExpA e1 + evalExpA (Cte 0)
--               ----------------
-- =                                       def evalExpA
-- evalExpA e1 + 0
-- ---------------
-- =                                       cero es neutro suma
-- evalExpA e1


-- CASO3: e1 && e2 != (Cte 0)

-- evalExpA (simplificarSuma e1 e2) = evalExpA e1 + evalExpA e2 ?

-- LI)

-- evalExpA (simplificarSuma e1 e2)
--          ----------------------
-- =                                       def simplificarSuma
-- evalExpA (Suma e1 e2)
-- ---------------------
-- =                                       def evalExpA
-- evalExpA e1 + evalExpA e2

-- LD)

-- evalExpA e1 + evalExpA e2

-- -- LEMA distributivo-multi-PROD

-- -- por ppio de extensionalidad
-- -- para todo expa1. para todo expa2. evalExpA (simplificarProd expa1 expa2) = evalExpA expa1 * evalExpA expa2 ?
-- -- voy a demostrar por casos:

-- CASO1 e1 = (Cte 0)

-- evalExpA (simplificarProd (Cte 0) e2) = evalExpA (Cte 0) * evalExpA e2 ?

-- LI)

-- evalExpA (simplificarProd (Cte 0) e2)
--          ---------------------------
-- =                                       def evalExpA
-- evalExpA (Cte 0)
-- ----------------
-- =                                       def evalExpA
-- 0 

-- LD)

-- evalExpA (Cte 0) * evalExpA e2
-- ----------------
-- =                                       def evalExpA
-- 0 * evalExpA e2
-- ---------------
-- =                                       cero es absorbente
-- 0

-- CASO2: e2 = (Cte 0)

-- evalExpA (simplificarProd e1 (Cte 0)) = evalExpA e1 * evalExpA (Cte 0) ?

-- LI)

-- evalExpA (simplificarProd e1 (Cte 0))
--          ---------------------------
-- =                                       def simplificarProd
-- evalExpA (Cte 0)

-- LD)

-- evalExpA e1 * evalExpA (Cte 0)
--               ----------------
-- =                                       def evalExpA
-- evalExpA e1 * 0
-- ---------------
-- =                                       cero es absorbente
-- 0




-- CASO3: e1 = (Cte 1)

-- evalExpA (simplificarProd (Cte 1) e2) = evalExpA (Cte 1) * evalExpA e2 ?

-- LI)

-- evalExpA (simplificarProd (Cte 1) e2)
--          ----------------------
-- =                                       def simplificarProd
-- evalExpA e2


-- LD)

-- evalExpA (Cte 1) * evalExpA e2
-- ----------------
-- =                                       def evalExpA
-- 1 * evalExpA e2
-- ---------------
-- =                                       1 es neutro en la multiplicacion
-- evalExpA e2

-- CASO4: e2 = (Cte 1)

-- evalExpA (simplificarProd e1 (Cte 1)) = evalExpA e1 * evalExpA (Cte 1) ?

-- LI)

-- evalExpA (simplificarProd e1 (Cte 1))
--          ---------------------------
-- =                                       def simplificarProd
-- evalExpA e1                             


-- LD)

-- evalExpA e1 * evalExpA (Cte 1)
--               ----------------
-- =                                       def evalExpA
-- evalExpA e1 * 1
-- ---------------
-- =                                       1 es neutro en la multiplicacion
-- evalExpA e1

-- CASO5: e1 && e2 != (Cte 1) && (Cte 0)

-- evalExpA (simplificarProd e1 e2) = evalExpA e1 * evalExpA e2 ?

-- LI)

-- evalExpA (simplificarProd e1 e2)
--          ---------------------------
-- =                                       def simplificarProd
-- evalExpA (Prod e1 e2)
-- ---------------------
-- =                                       def evalExpA
-- evalExpA e1 * evalExpA e2                         


-- LD)

-- evalExpA e1 * evalExpA e2


-- -- ii) cantidadDeSumaCero . simplificarExpA = const 0 ?
-- por ppio de extensionalidad
-- para todo expa. (cantidadDeSumaCero . simplificarExpA) expa = const 0 expa ?
-- por def (.)
-- para todo expa. cantidadDeSumaCero (simplificarExpA expa) = const 0 expa ?
-- por def de const
-- para todo expa. cantidadDeSumaCero (simplificarExpA expa) = 0 ?
-- sea exp de tipo ExpA, por ppio de induccion sobre la estructura de exp, es equivalente demostrar:

-- CASO BASE: exp = Cte n)

-- cantidadDeSumaCero (simplificarExpA (Cte n)) = 0 ?

-- CASO INDUCTIVO 1: exp = Suma expa1 expa2)

-- HI1) cantidadDeSumaCero (simplificarExpA expa1) = 0 !
-- HI2) cantidadDeSumaCero (simplificarExpA expa2) = 0 !
-- TI) cantidadDeSumaCero (simplificarExpA (Suma expa1 expa2)) = 0 ?

-- CASO INDUCTIVO 2: exp = Prod expa1 expa2)

-- HI1) cantidadDeSumaCero (simplificarExpA expa1) = 0 !
-- HI2) cantidadDeSumaCero (simplificarExpA expa2) = 0 !
-- TI) cantidadDeSumaCero (simplificarExpA (Prod expa1 expa2)) = 0 ?


-- CASO BASE: 

-- LI)

-- cantidadDeSumaCero (simplificarExpA (Cte n))
--                  -------------------------
-- =                                       def simplificarExpA
-- cantidadDeSumaCero (Cte n)
-- ------------------------
-- =                                       def cantidadDeSumaCero
-- 0

-- LD)

-- 0

-- CASO INDUCTIVO 1:

-- LI)

-- cantidadDeSumaCero (simplificarExpA (Suma expa1 expa2))
--                     ----------------------------------
-- =                                       def simplificarExpA.2
-- cantidadDeSumaCero (simplificarSuma (simplificarExpA expa1) (simplificarExpA expa2))
-- ------------------------------------------------------------------------------------
-- =                                       lema cantidadDeSumaCeroDistr
-- cantidadDeSumaCero (simplificarExpA expa1) + cantidadDeSumaCero (simplificarExpA expa2)
-- ---------------------------------------------------------------------------------------
-- =                                       HI1, HI2
-- 0 + 0
-- -----
-- =                                       def +
-- 0

-- LD)

-- 0

-- CASO INDUCTIVO 2:

-- LI)

-- cantidadDeSumaCero (simplificarExpA (Prod expa1 expa2))
--                    -----------------------------------
-- =                                       def simplificarExpA
-- cantidadDeSumaCero (simplificarProd (simplificarExpA expa1) (simplificarExpA expa2))
-- ------------------------------------------------------------------------------------
-- =                                       lema llegarAHipotesis
-- cantidadDeSumaCero (simplificarExpA expa1) * cantidadDeSumaCero (simplificarExpA expa2)
-- ------------------------------------------   ------------------------------------------
-- =                                       HI1, HI2
-- 0 * 0
-- -----
-- =                                       def *
-- 0


-- LD)

-- 0

-- ### LEMA llegarAHipotesis ###
-- por ppio de extensionalidad
-- para todo e1. para todo e2. cantidadDeSumaCero (simplificarProd e1 e2) = cantidadDeSumaCero e1 * cantidadDeSumaCero e2 ?
-- voy a demostrar por casos:

-- Caso1 = e1 = (Cte 0) y e2 es cualquier otra expresion

-- cantidadDeSumaCero (simplificarProd (Cte 0) e2) = cantidadDeSumaCero (Cte 0) * cantidadDeSumaCero e2 ?

-- LI)

-- cantidadDeSumaCero (simplificarProd (Cte 0) e2)
--                    ---------------------------
-- =                                       def simplificarProd
-- cantidadDeSumaCero (Cte 0)
-- -------------------------
-- =                                       def cantidadDeSumaCero
-- 0

-- LD)

-- cantidadDeSumaCero (Cte 0) * cantidadDeSumaCero e2
-- --------------------------
-- =                                       def cantidadDeSumaCero
-- 0 * cantidadDeSumaCero e2
-- -------------------------
-- =                                       0 absorbente
-- 0

-- Caso2 = e1 es cualquier otra expresion y e2 = (Cte 0)

-- cantidadDeSumaCero (simplificarProd e1 (Cte 0)) = cantidadDeSumaCero e1 * cantidadDeSumaCero (Cte 0) ?

-- LI)

-- cantidadDeSumaCero (simplificarProd e1 (Cte 0))
--                    ---------------------------
-- =                                       def simplificarProd
-- cantidadDeSumaCero (Cte 0)
-- --------------------------
-- =                                       def cantidadDeSumaCero
-- 0

-- LD)

-- cantidadDeSumaCero e1 * cantidadDeSumaCero (Cte 0)
--                         --------------------------
-- =                                       def cantidadDeSumaCero
-- cantidadDeSumaCero e1 * 0
-- -------------------------
-- =                                       0 es absorbente
-- 0

-- Caso3 = e1 es cualquier otra expresion y e2 = (Cte 1)

-- cantidadDeSumaCero (simplificarProd e1 (Cte 1)) = cantidadDeSumaCero e1 * cantidadDeSumaCero (Cte 1) ?

-- LI)

-- cantidadDeSumaCero (simplificarProd e1 (Cte 1))
--                     --------------------------
-- =                                       def simplificarProd
-- cantidadDeSumaCero e1

-- LD)

-- cantidadDeSumaCero e1 * cantidadDeSumaCero (Cte 1)
--                         --------------------------
-- =                                       def cantidadDeSumaCero
-- cantidadDeSumaCero e1 * 1
-- -------------------------
-- =                                       1 es neutro en multiplicacion
-- cantidadDeSumaCero e1


-- Caso4 = e1 = (Cte 1) y e2 es cualquier otra expresion

-- cantidadDeSumaCero (simplificarProd (Cte 1) e2) = cantidadDeSumaCero (Cte 1) * cantidadDeSumaCero e2 ?

-- LI)

-- cantidadDeSumaCero (simplificarProd (Cte 1) e2)
--                     --------------------------
-- =                                       def simplificarProd
-- cantidadDeSumaCero e2

-- LD)

-- cantidadDeSumaCero (Cte 1) * cantidadDeSumaCero e2
-- --------------------------
-- =                                       def cantidadDeSumaCero
-- cantidadDeSumaCero 1 * cantidadDeSumaCero e2
-- --------------------
-- =                                       1 es neutro en multiplicacion
-- cantidadDeSumaCero e2


-- Caso5 = e1 y e2 son cualquier expresion

-- cantidadDeSumaCero (simplificarProd e1 e2) = cantidadDeSumaCero e1 * cantidadDeSumaCero e2 ?

-- LI)

-- cantidadDeSumaCero (simplificarProd e1 e2)
--                    -----------------------
-- =                                       def simplificarProd
-- cantidadDeSumaCero (Prod e1 e2)
-- -------------------------------
-- =                                       def cantidadDeSumaCero
-- cantidadDeSumaCero e1 * cantidadDeSumaCero e2

-- LD)

-- cantidadDeSumaCero e1 * cantidadDeSumaCero e2


-- ### LEMA cantidadDeSumaCeroDistr###

-- por ppio de extensionalidad
-- para todo e1. para todo e2. cantidadDeSumaCero (simplificarSuma e1 e2) = cantidadDeSumaCero e1 + cantidadDeSumaCero e2 ?
-- voy a demostrar por casos:

-- CASO1 e1 = (Cte 0) y e2 = (Cte 0)

-- cantidadDeSumaCero (simplificarSuma (Cte 0) e2) = cantidadDeSumaCero (Cte 0) + cantidadDeSumaCero e2 ?

-- LI)

-- cantidadDeSumaCero (simplificarSuma (Cte 0) e2)
--                     --------------------------
-- =                                       def simplificarSuma
-- cantidadDeSumaCero e2

-- LD)

-- cantidadDeSumaCero (Cte 0) + cantidadDeSumaCero e2
-- -------------------------
-- =                                       def cantidadDeSumaCero
-- 0 + cantidadDeSumaCero e2
-- -------------------------
-- =                                       0 es neutro en la suma
-- cantidadDeSumaCero e2

-- CASO2 e2 = (Cte 0) y e1 != (Cte 0)

-- cantidadDeSumaCero (simplificarSuma e1 (Cte 0)) = cantidadDeSumaCero e1 + cantidadDeSumaCero (Cte 0) ?

-- LI)

-- cantidadDeSumaCero (simplificarSuma e1 (Cte 0))
--                     --------------------------
-- =                                       def simplificarSuma
-- cantidadDeSumaCero e1

-- LD)

-- cantidadDeSumaCero e1 + cantidadDeSumaCero (Cte 0)
--                         --------------------------
-- =                                       def cantidadDeSumaCero
-- cantidadDeSumaCero e1 + 0
-- -------------------------
-- =                                       0 es neutro en la suma
-- cantidadDeSumaCero e1


-- CASO3 e1 != (Cte 0) && e2 != (Cte 0)

-- cantidadDeSumaCero (simplificarSuma e1 e2) = cantidadDeSumaCero e1 + cantidadDeSumaCero e2 ?

-- LI)

-- cantidadDeSumaCero (simplificarSuma e1 e2)
--                     ----------------------
-- =                                       def simplificarSuma
-- cantidadDeSumaCero (Suma e1 e2)
-- -------------------------------
-- =                                       def cantidadDeSumaCero
-- sumarSiEsSumaCero e1 e2 + cantidadDeSumaCero e1 + cantidadDeSumaCero e2
-- -----------------------
-- =                                       def sumarSiEsSumaCero
-- 0 + cantidadDeSumaCero e1 + cantidadDeSumaCero e2
-- -------------------------------------------------
-- =                                       def 0 es neutro suma
-- cantidadDeSumaCero e1 + cantidadDeSumaCero e2

-- LD)

-- cantidadDeSumaCero e1 + cantidadDeSumaCero e2



data ExpS = CteS N | SumS ExpS ExpS | ProdS ExpS ExpS

ejemplito = SumS (CteS (S (S (S Z)))) (CteS (S( S (S Z))))

evalES :: ExpS -> Int
evalES (CteS n)        = evalN n
evalES (SumS es1 es2)  = evalES es1 + evalES es2
evalES (ProdS es1 es2) = evalES es1 * evalES es2

es2ExpA :: ExpS -> ExpA
es2ExpA (CteS n)        = Cte (evalN n)
es2ExpA (SumS es1 es2)  = Suma (es2ExpA es1) (es2ExpA es2)
es2ExpA (ProdS es1 es2) = Prod (es2ExpA es1) (es2ExpA es2)

expA2es :: ExpA -> ExpS
expA2es (Cte n)      = CteS (int2N n)
expA2es (Suma e1 e2) = SumS (expA2es e1) (expA2es e2)
expA2es (Prod e1 e2) = ProdS (expA2es e1) (expA2es e2)

i) evalExpA . es2ExpA = evalES ?
por ppio de extensionalidad.
para todo expsx. (evalExpA . es2ExpA) expsx = evalES expsx ?
por def de (.)
para todo expsx. evalExpA (es2ExpA expsx) = evalES expsx ?
sea exps un elemento de tipo ExpS, por ppio de induccion sobre la estructura de exps, es equivalente demostrar que:

CASO BASE: exps = (CteS n)

evalExpA (es2ExpA (CteS n)) = evalES (CteS n) ?

CASO INDUCTIVO 1: exps = (SumS e1 e2)

HI1) evalExpA (es2ExpA e1) = evalES e1 !
HI2) evalExpA (es2ExpA e2) = evalES e2 !
TI) evalExpA (es2ExpA (SumS e1 e2)) = evalES (SumS e1 e2) ?

CASO INDUCTIVO 2: exps = (ProdS e1 e2)

HI1) evalExpA (es2ExpA e1) = evalES e1 !
HI2) evalExpA (es2ExpA e2) = evalES e2 !
TI) evalExpA (es2ExpA (SumS e1 e2)) = evalES (SumS e1 e2) ?


CASO BASE: 

LI)

evalExpA (es2ExpA (CteS n))
        -------------------
=                               def es2ExpA
evalExpA (Cte (evalN n) )
-------------------------
=                               def evalExpA
evalN n

LD)

evalES (CteS n)
---------------
=                               def evalES
evalN n

CASO INDUCTIVO 1:

LI)

evalExpA (es2ExpA (SumS e1 e2))
          --------------------
=                               def es2ExpA
evalExpA (Suma (es2ExpA es1) (es2ExpA es2))
-------------------------------------------
=
evalExpA (es2ExpA es1) + evalExpA (es2ExpA es2)

LD)

evalES (SumS e1 e2)
-------------------
=                               def evalES
evalES es1 + evalES es2
----------   ----------
=                               HI1, HI2
evalExpA (es2ExpA es1) + evalExpA (es2ExpA es2)

CASO INDUCTIVO 2:

evalExpA (es2ExpA (ProdS e1 e2))
          --------------------
=                               def es2ExpA
evalExpA (Prod (es2ExpA es1) (es2ExpA es2))
-------------------------------------------
=
evalExpA (es2ExpA es1) * evalExpA (es2ExpA es2)

LD)

evalES (ProdS e1 e2)
-------------------
=                               def evalES
evalES es1 * evalES es2
----------   ----------
=                               HI1, HI2
evalExpA (es2ExpA es1) * evalExpA (es2ExpA es2)

evalES :: ExpS -> Int
evalES (CteS n)        = evalN n
evalES (SumS es1 es2)  = evalES es1 + evalES es2
evalES (ProdS es1 es2) = evalES es1 * evalES es2

es2ExpA :: ExpS -> ExpA
es2ExpA (CteS n)        = Cte (evalN n)
es2ExpA (SumS es1 es2)  = Suma (es2ExpA es1) (es2ExpA es2)
es2ExpA (ProdS es1 es2) = Prod (es2ExpA es1) (es2ExpA es2)

expA2es :: ExpA -> ExpS
expA2es (Cte n)      = CteS (int2N n)
expA2es (Suma e1 e2) = SumS (expA2es e1) (expA2es e2)
expA2es (Prod e1 e2) = ProdS (expA2es e1) (expA2es e2)

evalExpA :: ExpA -> Int
evalExpA (Cte n)            = n
evalExpA (Suma expa1 expa2) = evalExpA expa1 + evalExpA expa2
evalExpA (Prod expa1 expa2) = evalExpA expa1 * evalExpA expa2