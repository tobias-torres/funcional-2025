-- data GTree a = GNode a [GTree a]

-- sumGT :: GTree Int -> Int
-- sumGT (GNode x ts) = x + sum (map sumGT ts)

-- sumGTPO :: GTree Int -> Int
-- sumGTPO (GNode x ts) = x + aRaTyS ts

-- aRaTyS :: [GTree Int] -> Int -- Aplicar recursión a todos y sumar
-- aRaTyS [] = 0
-- aRaTyS (t:ts) = sumGTPO t + aRaTyS ts

data BST23 a = Cero 
             | Dos Int Int (BST23 a) a (BST23 a) 
             | Tres Int Int (BST23 a) a (BST23 a) a (BST23 a)

bst23 = Dos 3 4 (Dos 1 2 (Dos 0 1 Cero 20 Cero ) 21 Cero ) 30 (Dos 0 1 Cero 31 Cero)

bst23' = Tres 3 4 (Dos 1 2 Cero 32 Cero) 44 (Dos 1 2 Cero 33 Cero) 55 (Dos 0 1 Cero 3 Cero)

arbolSimple = Dos 2 3 (Dos 1 1 Cero 5 Cero) 10 (Dos 1 1 Cero 15 Cero)

-- Invariante de Representacion
-- en t = Dos h n t1 x t2
            -- h es la altura del t
            -- n es el size del t
            -- t1 y t2 tienen la misma altura
            -- todos los elementos de t1 son < x
            -- todos los elementos de t2 son > x
            -- t1 y t2 cumplen la inv. rep.

-- en t = Tres h n t1 x t2 y t3
            -- h es la altura del t
            -- n es el size de t
            -- t1, t2, t3 tienen la misma altura
            -- todos los elementos de t1 son < x
            -- todos los elementos de t2 son > x & < y
            -- x < y
            -- todos los elementos de t3 son > y
            -- t1, t2 y t3 cumplen la inv. rep.

-- 1. Dar el tipo y definicion de fold23 y rec23
-- 2. Definir las siguientes funciones sin recursion explicita

fold23 :: b -> (Int -> Int -> b -> a -> b -> b) -> (Int -> Int -> b -> a -> b -> a -> b -> b) -> BST23 a -> b
fold23 fc fd ft Cero                    = fc
fold23 fc fd ft (Dos h n t1 x t2)       = fd h n (fold23 fc fd ft t1) x (fold23 fc fd ft t2)
fold23 fc fd ft (Tres h n t1 x t2 y t3) = ft h n (fold23 fc fd ft t1) x (fold23 fc fd ft t2) y (fold23 fc fd ft t3)

rec23 :: b -> (Int -> Int -> BST23 a -> BST23 a -> b -> a -> b -> b) -> (Int -> Int -> BST23 a -> BST23 a -> BST23 a -> b -> a -> b -> a -> b -> b) -> BST23 a -> b
rec23 fc fd ft Cero                    = fc 
rec23 fc fd ft (Dos h n t1 x t2)       = fd h n t1 t2 (rec23 fc fd ft t1) x (rec23 fc fd ft t2)
rec23 fc fd ft (Tres h n t1 x t2 y t3) = ft h n t1 t2 t3 (rec23 fc fd ft t1) x (rec23 fc fd ft t2) y (rec23 fc fd ft t3)

inOrder :: BST23 a -> [a]
inOrder = fold23 [] (\h n xs x ys -> xs ++ [x] ++ ys) (\h n xs x ys y zs -> xs ++ [x] ++ ys ++ [y] ++ zs)

cantElem :: BST23 a -> Int
cantElem = fold23 0 (\h n t1 x t2 -> 1 + t1 + t2) (\h n t1 x t2 y t3 -> 1 + t1 + t2 + t3)

altura :: BST23 a -> Int
altura = fold23 0 (\h n t1 x t2 -> 1 + max t1 t2) (\h n t1 x t2 y t3 -> 1 + max t1 (max t2 t3))

minElem :: BST23 a -> a
minElem = rec23 (error "No hay elemento Minimo")
                    (\h n t1 t2 m1 x m2 -> case t1 of 
                                        Cero -> x
                                        _ -> m1) 
                    (\h n t1 t2 t3 m1 x m2 y m3 -> case t1 of 
                                        Cero -> x
                                        _ -> m1)

maxElem :: BST23 a -> a
maxElem = rec23 (error "No hay elemento Maximo")
                (\_ _ _ t2 _ x m2 -> case t2 of
                                        Cero -> x
                                        _    -> m2)
                (\_ _ _ _ t3 m1 _ _ y m3 -> case t3 of
                                        Cero -> y
                                        _    -> m3)

-- search :: Ord a => a -> BST23 a -> a
-- search e Cero                    = error "no hay elemento"
-- search e (Dos h n t1 x t2)       = if e == x then x else if e < x then search e t1 else search e t2
-- search e (Tres h n t1 x t2 y t3) = if e == x
--                                     then x
--                                     else if e == y
--                                         then y
--                                         else if e < x
--                                             then search e t1
--                                             else if e < y then search e t2
--                                             else search e t3 

search :: Ord a => a -> BST23 a -> a
search e = fold23 (error "No existe el elemento")
                  (\h n e1 x e2      -> if e == x then x else if e < x then e1 else e2 )
                  (\h n e1 x e2 y e3 -> if e == x
                                            then x
                                            else if e == y
                                                then y
                                                else if e < x
                                                    then e1
                                                    else if e < y then e2
                                                         else e3  )
