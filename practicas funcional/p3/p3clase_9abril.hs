-- i

doble x = 2 * x

suma x y = x + y


-- suma . doble 2

-- compose suma (doble 2) -- Me falta un argumento para poder resolver el compose, doble 2 es de tipo Int, por lo tanto g deberia ser una funcion y no un Int

-- compose :: (b -> c) -> ((a -> b) -> (a -> c))
-- suma    :: Int -> (Int -> Int)
-- --------------------------------------------------
-- compose suma :: (a -> Int) -> (a -> (Int -> Int))
-- doble 2      :: Int

-- (suma . doble) 2

-- (compose suma doble) 2

-- compose suma doble 2

-- suma (doble 2)

-- ii

-- doble . (suma 2) 

-- doble . (suma 2)

-- doble (suma 2) :: No tiene tipo

-- doble suma 2

-- doble . doble 2 

