data BST23 a = Cero 
             | Dos Int Int (BST23 a) a (BST23 a) 
             | Tres Int Int (BST23 a) a (BST23 a) a (BST23 a)

bst23 = Dos 3 4 (Dos 1 2 (Dos 0 1 Cero 20 Cero ) 21 Cero ) 30 (Dos 0 1 Cero 31 Cero)

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
            -- todos los elementos de t2 son > x ; < y
            -- x < y
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

minElem :: BST23 a -> a
minElem = rec23 error ("el bst esta vacio")
                undefined
                undefined