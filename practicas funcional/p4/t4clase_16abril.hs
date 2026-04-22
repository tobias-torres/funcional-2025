-- (.)(.)(.) :: (b3 -> c3) -> (a2 -> a3 -> b3) -> a2 -> a3 -> c3

-- -- 1
-- (.)    :: (b1 -> c1) -> ((a1 -> b1) -> (a1 -> c1))
-- (.)    :: (b2 -> c2) -> ((a2 -> b2) -> (a2 -> c2))
-- ------------------------------------------------- b1 <-- (b2 -> c2), c1 <-- ((a2 -> b2) -> (a2 -> c2))
-- (.)(.) :: (a1 -> (b2 -> c2)) -> (a1 -> ((a2 -> b2) -> (a2 -> c2)))
-- (.)    :: (b3 -> c3) -> ((a3 -> b3) -> (a3 -> c3))
-- ------------------------------------------------------------------ a1 <-- (b3 -> c3), b2 <-- (a3 -> b3), c2 <-- (a3 -> c3)
-- (.)(.)(.) :: (b3 -> c3) -> (a2 -> a3 -> b3) -> a2 -> a3 -> c3

-- 2

-- (.)(.)(.) :: (b3 -> c3) -> (a2 -> a3 -> b3) -> a2 -> a3 -> c3
-- (.)(.)(.) f g x y = f (g x y)

comp3 :: (b3 -> c3) -> (a2 -> a3 -> b3) -> a2 -> a3 -> c3
comp3 f g x y = f (g x y)

-- def (.) f g x = f (g x)

-- -- 3 

-- (.)(.)(.) succ div = es parcial

--         f   g    x
-- (((((.)(.))(.)) succ) div) 2 0
-- ->                          def (.), f <- (.), g <- (.), x <- succ
-- (.) ((.) succ) div 2 0
-- ->                          def (.), f <- ((.) succ), g <- div, x <- 2
-- (.) succ (div 2) 0
-- ->                          def (.), f <- succ, g <- (div 2), x <-0
-- succ ((div 2) 0)

-- 4

-- (.)(.)(.) 

-- (yTambien b1) b2 = b1 && b2

yTambien :: Bool -> (Bool -> Bool)
yTambien b = f 
        where f b' = if b then b' else b

yTambien' :: Bool -> (Bool -> Bool)
yTambien' b = if b then (\x -> x) else (\x -> False)

-- yTambien' es estricta, ya que necesita si o si del segundo parametro para darte el resultado