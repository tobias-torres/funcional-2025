-- Funciones definidas

doble :: Num a => a -> a
doble x = x + x 

cuadruple x = 4 * x 

twice f = g
  where g x = f (f x)

-- 1.

resultadoCuatroD = id doble 2

resultadoCuatro = doble (doble 1)

resultadoCuatroI = twice doble 1

resultadoCuatroII = cuadruple 1

resultadoCuatroIII = (\x -> 4 * x) 1

resultadoCuatroV = \x -> 4

resultadoCuatroIV = (\x -> twice doble x) 1

-- 2.

-- doble (doble 2)
-- ---------------
-- --->                       def doble. x <- (doble 2)
-- (doble 2) + (doble 2)
-- ---------   ---------
-- --->                       def doble. x <- 2
-- (2 + 2) + (2 + 2)
-- -------   -------
-- --->                       suma
-- 4 + 4
-- -----
-- --->                       suma
-- 8

-- -- 3.

-- cuadruple 2
-- -----------
-- --->                       def cuadruple. x <- 2
-- 4 * 2

-- cuadruple 2 (cuadruple 2)
-- -----------
-- --->                       def cuadruple. x <- (cuadruple 2)
-- 4 * (cuadruple 2)
--     -----------
-- --->                       def cuadruple. x <- 2
-- 4 * (4 * 2)
-- -----------
-- --->                       multiplicación
-- 4 * 8
-- -------
-- --->                       multiplicación
-- 32

-- 4.

triple x = 3 * x

succ x = 1 + x

sumarDos x = 2 + x

-- 5.

-- twice succ = sumarDos

-- -- IZQ
-- twice succ 1
-- ----------
-- ->                     def de twice, f -> succ 
-- g 1
-- ----
-- ->                     def de g x = (succ (succ x))
-- succ (succ 1)
--       -------
-- ->                     def de suma
-- succ 2
-- -------
-- ->                     def de succ, x -> 1
-- 1 + 2
-- -----                 
-- 3

-- -- DER

-- sumarDos 1
-- ----------
-- ->                      def sumarDos, x -> 1
-- 2 + 1
-- -----
-- ->                      def de suma
-- 3

-- -- 6.

-- succ = (+) 1

-- 2 + 2 = 4

-- 7
--           f      g    x
-- ((twice twice) doble) 3
-- --------------
-- ->                     def de twice, f <- twice
-- (g doble) 3
-- --------
-- ->                     def de g, x <- doble
-- (twice (twice doble)) 3
-- ---------------------
-- ->                     def de twice, f <- twice doble
-- g' 3
-- ----
-- ->                     def de g', x <- 3
-- twice doble((twice doble) 3)
--              -----------
-- ->                     def de twice, f <- doble
-- twice doble (g'' 3)
--              -----
-- ->                     def de g'', x <- 3
-- twice doble (doble (doble 3))
--                     -------
-- ->                     def de doble, x <- 3
-- twice doble (doble (3 + 3))
--                     -----
-- ->                     def suma
-- twice doble (doble 6)
--             --------
-- ->                     def de doble, x <- 6
-- twice doble (6 + 6)
--             ------
-- ->                     def suma
-- twice doble 12
-- -----------
-- ->                     def twice, f <- doble 
-- g''' 12
-- -------
-- ->                     def g''', x <- 12
-- doble(doble 12)
--       --------
-- ->                     def doble, x <- 12
-- doble (12 + 12)
--       --------
-- ->                     def +
-- doble 24
-- --------
-- ->                     def doble, x <- 24
-- 24 + 24
-- -------
-- ->                     def +
-- 48


analizar x = 
      let resultado = if x > 0 then "positivo" else "negativo"
      in case resultado of
            "positivo" -> "El número es positivo"
            "negativo" -> "El número es negativo"

-- 8.

tripleI = \x -> 3 * x

tripleII = \x -> x + x + x

tripleIII = \x -> twice (+ x) x

succI = \x -> x + 1

succII = \x -> (+1) x

sumarDosI = \x -> 2 + x

sumarDosII = \x -> (+2) x

twiceI = \f x -> f (f x)

twiceII = \f x -> f (f (f (f x)))

twiceTwiceI = \f -> \x -> f (f (f ( f x)))

-- 10

-- f x = let (y,z) = (x,x) in y ---> f x = x

-- f (x,y) = let z = x + y in g (z,y) where g (a,b) = a - b ---> f (x, y) = (x + y) - y  = f x = x

-- f p = case p of (x,y) -> x ---> f p = fst p

-- f = \p -> let (x,y) = p in y ---> f p = snd p