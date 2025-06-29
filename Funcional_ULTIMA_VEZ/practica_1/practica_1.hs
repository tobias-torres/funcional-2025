-- Ejercicio 1

twice f = g
    where g x = f (f x)

doble x = 2 * x

cuadruple x = 4 * x
----------------------------------------------------------------------------------------------------

-- Ejercicio 1

siempreCuatro = \x -> 4

siempreCuatroBis = \x -> 4 * 1

sumaCuatro = twice doble 2

sumaCuatro' = id doble 2

sumaCuatro'' = id cuadruple 1

sumaCuatro''' = doble (doble 1)

-- Ejercicio 2

-- doble (doble 2)
-- ---------------
-- ->                          def de doble, x <-- (doble 2)
-- 2 * (doble 2)
--     --------
-- ->                          def de doble, x' <-- 2
-- 2 * (2 * 2)
--     ------
-- ->                          def de (*)
-- 2 * 4
-- -----
-- ->                          def de (*)
-- 8

-- -- Ejercicio 3

-- cuadruple 2​
-- -----------
-- ->                          def de cuadruple, x <-- 2
-- 4 * 2
-- -----
-- ->                          def de (*)
-- 8

-- cuadruple (cuadruple 2)
-- -----------------------
-- ->                          def de cuadruple, x <-- (cuadruple 2)
-- 4 * (cuadruple 2)
--     ------------
-- ->                          def de cuadruple, x <-- 2
-- 4* (4 * 2)
--     -----
-- ->                          def de (*)
-- 4*8
-- ---
-- ->                          def de (*)
-- 32

-- Ejercicio 4

triple x = 3 * x

succ x = 1 + x

sumarDos x = 2 + x

-- -- Ejercicio 5

-- (twice succ) 1
-- ------------
-- =                                    def de twice, f <-- succ
-- g 1
-- ----                          
-- =                                    def de g, x <-- 1
-- succ (succ 1)
--      --------
-- =                                    def de succ
-- succ (1 + 1)
--      ------
-- =                                    def de +
-- succ 2
-- ------
-- =                                    def de succ
-- 1 + 2
-- -----
-- =                                    def de +
-- 3


-- sumarDos 1
-- ----------
-- =                                   def de sumarDos, x <-- 1
-- 2 + 1
-- -----
-- =                                   def de +
-- 3              

-- -- Ejercicio 7

-- ((twice twice) doble) 3
--   -----------
-- =                                   def de twice, f <-- twice
-- (g doble) 3
-- --------
-- =                                   def de g, x <-- doble
-- twice(twice doble) 3
-- --------------------
-- =                                   def twice, f' <-- twice doble
-- g 3
-- ---
-- =                                   def de g, x <-- 3
-- twice doble((twice doble) 3)
-- --------------------------
-- =                                   def de twice, f'' <-- doble
-- g ((twice doble) 3)
-- -----------------
-- =                                   def de g, x'' <-- (twice doble 3)
-- doble(doble((twice doble) 3))
--             -------------
-- =                                   def de twice, f''' <-- doble
-- doble(doble(g 3))
--             ---
-- =                                   def de g, x''' <-- 3
-- doble(doble(doble(doble 3)))
-- ---------------------------
-- =
-- 48

-- Ejercicio 8

tripleLambda = \x -> 3 * x

succLambda = \x -> 1 + x

sumarDosLambda = \x -> 2 + x

twiceLambda = \f -> \x -> f (f x)
twiceLambda' = \f x -> f (f x)

twicetwiceLambda = \f -> \x -> f (f (f (f x))) -- uso una funcion anonima como resultado de una funcion anonima!!!
twicetwiceLambda' = \f x -> f (f (f (f x)))

-- Ejercicio 9

f x = let (y,z) = (x,x) in y

fRedefinida x = x

f' (x,y) = let z = x + y in g (z,y) where g (a,b) = a - b

f'Redefinida (x,y) = x

f'' p = case p of (x,y) -> x

f''Redefinida p = fst p
f''RedefinidaBis = \p -> fst p

f''' = \p -> let (x,y) = p in y

f'''Redefinida p = snd p
f'''RedefinidaBis = \p -> snd p