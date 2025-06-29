-- PRACTICA 2 

-- Ejercicio 1

first :: (a,b) -> a
first (x,y) = x 

apply :: (a -> b) -> a -> b
apply f = g 
  where g x = f x 

twice :: (a -> a) -> a -> a
twice f = g 
  where g x = f (f x) 

doble :: Int -> Int
doble x = x + x 

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x) 

uflip :: ((b,a) -> c) -> (a, b) -> c
uflip f = g 
  where g p = f (swap p)

-- Funciones del ejercicio 3

const' x = g  
  where g y = x 

appDup f = g 
  where g x = f (x, x)

appFork (f, g) = h 
  where h x = (f x, g x)

appPar (f, g) = h 
  where h (x, y) = (f x, g y)

appDist f = g 
  where g (x, y) = (f x, f y)

flip' f = h 
  where h x = k where k y = (f y) x

subst f = h 
  where h g = k where k x = (f x) (g x)


-- Ejercicio 2
           -- a      b
-- apply :: (a -> b) -> a -> b
-- first :: (a,b) -> a
-- ---------------------------- a -> (a, b) , b -> a
-- apply first :: (a,b) -> a

-- first         :: (a, b) -> a
-- (swap, uflip) :: ((a, b) -> (b, a), ((b,a) -> c) -> (a, b) -> c)
-- -----------------------------------------
-- first (swap, uflip) :: (a, b) -> (b, a)

-- twice :: (a -> a) -> (a -> a)
-- doble :: Int -> Int
-- -------------------------------------------- 
-- twice doble :: ( Int -> Int )

-- twice       :: (a' -> a') -> (a' -> a')
-- twice       :: (a -> a) -> (a -> a)         a' <- (a -> a), 
-- --------------------------------------------
-- twice twice :: (a -> a) -> (a -> a)

-- twice       :: (a -> a) -> (a -> a)
-- uflip       :: ((b,a) -> c) -> (a, b) -> c   a' <- ((b,a) -> c) , a' <- (a, b) -> c
-- --------------------------------------------   
-- twice uflip :: ((a, a) -> b) -> (a, a) -> b

-- twice      :: (a -> a) -> (a -> a)
-- swap       :: (a, b) -> (b, a)
-- --------------------------------------------
-- twice swap :: (a, a) -> (a, a)

-- uflip      :: ((b,a) -> c) -> (a, b) -> c
-- swap       :: (a, b) -> (b, a)
-- -------------------------------------------- 
-- uflip swap ::  

-- h. (twice twice) swap

-- Ejercicio 4

-- a. 1 && 2 == 2 No tiene tipo
-- b. 1 + if 3 < 5 then 3 else 5 :: b
-- c. let par = (True, 4) 
--     in (if first par then first par else second par) :: error de tipo, no puede haber un par de distinto tipo
-- d. (doble doble) 5 :: error de tipo, doble toma un numero no una funcion
-- e. doble (doble 5) :: Int
-- f. twice first :: Error de tipo, no se pueden crear infinitos pares para que de bien el tipo de twice
-- g. (twice doble) doble :: error de tipo, doble no toma un tipo funcion
-- h. (twice twice) first :: mismo error que en el D
-- i. apply apply :: (a -> b) -> a -> b

-- Ejercicio 5

-- a. Bool
expresion1 = True
expresion2 = False
expresion3 = True && False
expresion4 = 3 > 6
expresion5 = 10 == 3
expresion6 = 10 /= 3
-- b. (Int, Int) 


-- c. Char -> Int 
-- d. (Int, Char) -> Bool 
-- e. (Int -> Int) -> Int 
-- f. (Bool -> Bool, Int) 
-- g. a -> Bool 


-- Ejercicio 6

-- \p -> let (f, g) = p in \x -> (f x, g x) = "appFork" (f, g) = h where h x = (f x, g x)

-- \f -> (\g -> (\x -> f x (g x)) = 

-- \f -> (\x -> (\y -> (f y) x) = "flip" f = h where h x = k where k y = (f y) x

-- \f -> (\px -> let (x, y) = px in (f x, f y)) = appDist f = g where g (x, y) = (f x, f y)

-- \x -> (\y -> x) = const x = g where g y = x

-- \pf -> let (f, g) = pf in \px -> let (x, y) = px in (f x, g y) = appPar​ ​(f, g) = h where h (x, y) = (f x, g y) 

-- \f -> (\x -> f (x, x)) = appDup f = g where g x = f (x, x)

-- Ejercicio 7

ejA = appFork (id,id)     -- es equivalente a ejC
ejA' = \f x -> (f x, f x) -- es equivalente a ejC

ejB = \f -> appDup (appDist f) -- :: (a -> b) -> a -> (b,b), -- es equivalente a ejB 
ejB' = \g y -> (g y, g y) -- es equivalente a ejB

ejC  = appDup id    -- es equivalente a ejA
ejC' = \x -> (x,x)  -- es equivalente a ejA

ejD  = appDup appFork  -- es equivalente a ejB
ejD' = \f x -> (f x, f x) -- por lo tanto es equivalente a \f -> appDup (appDist f)

ejE  = flip (appDup const) es identica a ejF
-- ejE' = \f appDup const -> (f const) appDup

ejF = const (appDup id) es identica a esta funcion
