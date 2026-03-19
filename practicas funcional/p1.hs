-- Funciones definidas

doble :: Num a => a -> a
doble x = x + x 

cuadruple x = 4*x 

twice f = g 
    where g x = f (f x)


-- 1.

resultadoCuatroD = doble 1 + doble 1

resultadoCuatro = doble (doble 1)

resultadoCuatroI = twice doble 1

resultadoCuatroII = cuadruple 1

resultadoCuatroIII = (\x -> 4 * x) 1

resultadoCuatroIV = (\x -> twice doble x) 1

-- 2.

doble (doble 2)
---------------
--->                       def doble. x <- (doble 2)
(doble 2) + (doble 2)
---------   ---------
--->                       def doble. x <- 2
(2 + 2) + (2 + 2)
-------   -------
--->                       suma
4 + 4
-----
--->                       suma
8

-- 3.

cuadruple 2
-----------
--->                       def cuadruple. x <- 2
4 * 2

cuadruple 2 (cuadruple 2)