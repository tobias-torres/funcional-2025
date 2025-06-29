-- ejercicio 1

-- Prop: doble = \x -> 2 * x ?
-- Dem: Por ppio de extensionalidad, es equivalente demostrar que 
-- para todo x. doble x = (\x -> 2 * x) x ?
-- sea n un numero cualquiera. Se vera que doble n = (\x -> 2 * x) n ?

-- IZQ)

-- doble n
-- -------
-- =                                           def de doble, x <- n
-- 2 * n

-- Der)

-- (\x -> 2 * x) n
-- ----------------
-- =                                           def beta reduccion, x <- n
-- 2 * n

-- -- Prop: compose doble doble = cuadruple
-- Dem: Por ppio de extensionalidad, es equivalente demostrar que 
-- para todo x. compose doble doble x = cuadruple x
-- sea n un numero cualquiera. Se vera que compose doble doble n = cuadruple n ?

-- IZQ)

-- doble x = 2 * x

-- compose :: (b -> c) -> (a -> b) -> a -> c
-- compose f g x = f (g x) 

-- compose doble doble n
-- ---------------------
-- =                                           def compose, f <- doble, g <- doble, x <- n
-- doble (doble n)
--       ----------
-- =                                           def de doble, x <- n
-- doble (2 * n)
-- -------------
-- =                                           def de doble, x <- (2 * n)
-- 2 * (2 * n)
-- -----------
-- =                                           por aritm
-- 4 * n

-- DER)

-- cuadruple n
-- -----------
-- =                                           def de cuadruple, x <- n
-- 4 * n

-- Ejercicio 2

-- Dem) x && y = not ((not x) || (not y)) ?
-- por ppio de extensionalidad es equivalente demostrar que:
-- para todo x. para todo y. x && y = not ((not x) || (not y))
-- sean b1 y b2 dos elementos de tipo Bool. Se vera que b1 && b2 = not ((not b1) || (not b2))
-- voy a demostrar por casos

-- CASO 1 - b1: True , b2: False

-- IZQ)

-- b1 && b2 
-- --------
-- =                             def de &&
-- False

-- DER)

-- not ((not b1) || (not b2))
--       -------------------
-- =                             def de ||
-- not (True)
-- ----------
-- =                             def de not
-- False

-- CASO 2 - b1: False, b2: True

-- IZQ)

-- b1 && b2
-- --------
-- =                             def de &&
-- False

-- DER)

-- not ((not b1) || (not b2))
--       ------      ------
-- =                             def de not
-- not (True || False)
-- -------------------
-- =                             def de ||
-- not True
-- --------
-- =                             def de not
-- False

-- CASO 3 - b1: True, b2: True

-- IZQ)

-- b1 && b2
-- --------
-- =                             def de &&
-- True

-- DER)

-- not ((not b1) || (not b2))
--       ------      ------
-- =                             def de not
-- not (False || False)
--       -------------
-- =                             def de ||
-- not False
-- ---------
-- =                             def de not
-- True

-- CASO 4 - b1: False, b2: False

-- IZQ)

-- b1 && b2
-- --------
-- =                             def de &&
-- False


-- DER)

-- not ((not b1) || (not b2))
--       ------      ------
-- =                             def de not
-- not (True || True)
--       -------------
-- =                             def de ||
-- not True
-- ---------
-- =                             def de not
-- False

-- Ejercicio 3) Demostrar las siguientes propiedades:

-- Prop) curry suma' = suma?
-- Dem) por ppio de extensionalidad es equivalente demostrar que:
-- ¿para todo x. para todo y. curry suma' x y = suma x y?
-- sean n y m dos numeros. Se vera que curry suma' n m = suma n m

-- Izq)

-- curry suma' n m
-- ---------------
-- =                                               def de curry
-- suma' (n, m)
-- ------------
-- =                                               def de suma'
-- n + m

-- Der)

-- suma n m
-- --------
-- =                                               def de suma
-- n + m


suma :: Int -> Int -> Int
suma x y = x + y

suma' :: (Int, Int) -> Int
suma' (x,y) = x + y

curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

flip' :: (b -> a -> c) -> a -> b -> c
flip' f x y = f y x 

-- Ejercicio 4)

-- Prop) uncurry (flip const) = snd ?
-- Dem) por el ppio de extensionalidad, es equivalente demostrar que:
-- para todo (x,y). uncurry (flip const) (x, y) = snd (x, y)?
-- sea (n, m) de tipo par de elementos. Se vera que uncurry (flip const) (n, m) = snd (n, m)

-- IZQ)
            -- f     x   y
-- uncurry (flip const) (n, m)
--          -------------------
-- =                             def uncurry
-- uncurry const (m, n)
-- --------------------
-- =                             def de uncurry
-- const m n
-- ---------
-- =                             def de const
-- m

-- -- DER)

-- snd (n, m)
-- -----
-- =                             def snd
-- m

-- 7.a

-- cuadruple' :: Int -> Int
-- i 
cuadruple = (doble . doble)

cuadruple' = ((* 2) . (* 2)) -- aplicacion parcial

cuadruple'' = (subst . const) doble doble

-- ii

doble' :: Int -> Int
doble' = (succ . succ)

-- iii

twice' = (id . id) 

-- iv

many 0 f = (id . id)
many n f = f . (many (n-1) f)

twice :: (a -> a) -> a -> a
twice f x = f (f x)

doble :: Int -> Int
doble x = 2 * x

compose f g x = f (g x)

subst = \f g x -> (f x) (g x)

-- 7. b

i. para todo f. para todo g. f . g = compose f g -- podria haber reducido antes de desplegar esto pero fueye
para todo f. para todo g. para todo x. (f . g) x = compose f g x
sean f' y g', funciones y x' de un elemento cualquiera. Se vera que:

(f' . g') x' = compose f' g' x' ?

Lado Izq:

(f' . g') x'
------------
=                   def de (.)
f'(g' x')

Lado Der:

compose f' g' x'
---------------
=                   def de compose
f'(g' x')

ii. Prop: swap . swap = id?
Dem) por ppio de extensionalidad, es equivalente demostrar que:
para todo (x, y) . (swap . swap) (x, y) = id (x, y)?
por def (.)
para todo (x, y) . swap (swap (x, y)) = id (x, y)?
por def de id
para todo (x, y). swap (swap (x, y)) = (x, y)?
sean (n, m) un par de elementos cualquiera. Se vera que:

swap (swap (x, y)) = (x, y)?

Lado Izq:

swap (swap (x, y)) 
     ------------
=                       def de swap
swap (y, x)
-----------
=                       def de swap
(x, y)

Lado Der:

(x, y)

iii. para todo f. para todo g. para todo h. f . (g . h) = (f . g) . h
                                                --   f       g    x       f      g  x
para todo f. para todo g. para todo h. para todo x. (f . (g . h)) x = ((f . g) . h) x
                                                    ---------------   ---------------
por def de (.)                                  --      f   g  x     f   g    x
para todo f. para todo g. para todo h. para todo x. f ((g . h) x) = (f . g) (h x)
                                                        --------     -------------
por def de (.)
para todo f. para todo g. para todo h. para todo x. f (g (h x)) = f (g (h x))

son iguales

iv. curry . uncurry = id -- ya lo hice en la carpeta

v. para todo f. appAssoc f = f . assoc?
para todo f. para todo x. appAssoc f (x, y) = (f . assoc) (x, y)?
por def de (.)
para todo f. para todo x. appAssoc f (x, y) = f (assoc (x, y))?
sea f' un tipo funcion y (n, m) un par de elementos cualquiera. Se vera que:

appAssoc f' (n, m) = f' (assoc (n, m))?

Lado Izq:

appAssoc f' (n, m)
------------------
=                       def de appAssoc
f' (assoc (n, m))

llegue al lado izquierdo

-- c.

-- i. doble . doble = cuadruple

Lado Izq:

(doble . doble)
---------------
=                       por propiedad f . g = compose f g
compose doble doble
-------------------
=                       por propiedad compose doble doble = cuadruple
cuadruple

llegue al lado derecho

-- ii. para todo f'. curry (uncurry (curry f')) = curry f'

Lado izq)

curry (uncurry (curry f'))
        -----------------
=                               por propiedad uncurry (curry f') = f'
curry f'

llegue al lado derecho

-- iii. para todo f. appAssoc (uncurry (uncurry f)) = (uncurry . uncurry) f . assoc

Lado Izq)

appAssoc (uncurry (uncurry f))

Lado Der)

(uncurry . uncurry) f . assoc