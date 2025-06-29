import Data.Char (toLower)

-- Ejercicio 1

suma x y = x + y

doble x = 2 * x

udiv (x,y) = div x y -- funcion parcial, no se puede dividir por 0

udivE (x,0) = error "No puedo dividir por 0" -- parcial, no se puede dividir por 0
udivE (x,y) = div x y 

udivH = uncurry div -- Parcial, no se puede dividir por 0 

succ' x = x + 1 -- Funcion Total

succH = suma 1 -- Funcion Total

porLaMitad = flip div 2 -- Funcion Total

-- conDieresis 'u' = 'ü' -- Funcion Parcial

-- conDieresisB 'u' = 'ü' 
conDieresisB c   = conDieresisB c -- se cuelga la funcion, es parcial

-- conTildePM 'a' = 'á' 
-- conTildePM 'e' = 'é' 
-- conTildePM 'i' = 'í' 
-- conTildePM 'o' = 'ó' 
-- conTildePM 'u' = 'ú' -- funcion parcial 

-- esVocal :: Char -> Bool
-- esVocal c = toLower c `elem` "aeiou"

-- conTildeE c = if esVocal c then conTildePM c else error "El valor recibido no es vocal" -- es una funcion parcial


-- esMinuscula :: Char -> Bool
-- esMinuscula c = c == (toLower c)

-- conTilde c = if esVocal c && esMinuscula c 
--               then conTildePM c 
--               else c -- es Total

-- ejercicio 2

-- udiv = udivH

-- succ' x = succH 

-- Ejercicio 3

-- a

twice = \f -> \x -> f (f x) 

-- twice doble
-- -----
-- =                   def de twice
-- \f -> \x -> f (f x) doble
-- -------------------------
-- =                   beta reduccion , f = doble
-- \x -> doble (doble x)

-- 2 redex


-- -- b

-- twice doble 2
-- -----
-- =                    def de twice                
-- \f -> \x -> f (f x) doble 2
-- -------------------------
-- =                    beta reduccion, f = doble
-- \x -> doble (doble x) 2
-- ---------------------
-- =                    beta reduccion, x = 2
-- doble (doble 2)
--        -------
-- =                   def de doble
-- doble (2 * 2)
--       -------
-- =                   def de *
-- doble 4
-- -------
-- =                   def de doble
-- 2 * 4
-- -----
-- =                       def de *
-- 8

-- 7 redex

-- -- c

-- twice
-- -----
-- =                       def de twice
-- \f -> \x -> f (f x)

-- 1 redex

-- -- 4

twice' f = g 
  where g x = f (f x)


-- -- a.

-- twice doble
-- -----------
-- =                       def de twice, f -> doble
-- g

-- 1 redex

-- -- b.

-- twice doble 2
-- -----------
-- =                       def de twice, f -> doble
-- g 2
-- ---
-- =                       def de g, x -> 2
-- doble (doble 2)
--       --------
-- =                       def doble
-- doble (2 * 2)
--       ------
-- =                       def de *
-- doble 4
-- -------
-- =                       def de doble
-- 2 * 4
-- -----
-- =                       def de *
-- 8

-- -- c 

-- twice

-- 0 redex

-- 5

twice'' f x = f (f x)

-- -- a

-- twice doble

-- 0 redex

-- -- b

-- twice doble 2
-- -------------
-- =                       def twice, f -> doble, x -> 2
-- doble (doble 2)
--       --------
-- =                       def doble
-- doble (2 * 2)
--       ------
-- =                       def de *
-- doble 4
-- -------
-- =                       def de doble
-- 2 * 4
-- -----
-- =                       def de *
-- 8

-- -- c

-- twice

-- 0 redex

-- 6

-- a

a :: a
x = x