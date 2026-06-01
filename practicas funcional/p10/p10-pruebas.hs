-- import Data.Char (toUpper)

-- foldr :: (a -> b -> b) -> b -> [a] -> b

succs :: [Int] -> [Int]
sucss []     = []
succs (x:xs) = 1 + x : succs xs

-- uppers :: [Char] -> [Char]
-- uppers []     = []
-- uppers (x:xs) = toUpper x : uppers xs

test :: [Int] -> [Bool]
test []     = []
test (x:xs) = (x == 0) : test xs

takeF :: Int -> ([a] -> [a])
takeF n []     = []
takeF n (x:xs) = if n==0 then [] else x : takeF (n-1) xs

-- foldr :: (a -> b -> b) -> b -> [a] -> b

-- mi tipo b es de Int -> [a], porque takeF (n-1) xs, entonces
--          :: (a ->       b      ->        b)          :: b       :: [a]      :: b
-- foldr :: (a -> (Int -> [a]) -> (Int -> [a])) -> (Int -> [a]) -> [a] -> Int -> [a]

-- Paso 5: Evaluando lo que ya tenemos
-- En tu código, vos escribís esto: foldr ctk (\n -> []).
-- Al hacer esto, ya le estás entregando a foldr sus dos primeros parámetros:

-- La función de paso: ctk ocupa el lugar de (a -> (Int -> [a]) -> (Int -> [a])).

-- El caso base: (\n -> []) ocupa el lugar de (Int -> [a]).

-- Como ya le dimos esos dos ingredientes, los "tachamos" de la firma. ¿Qué nos queda? Nos queda lo que el foldr todavía está esperando recibir:
-- [a] -> Int -> [a]

-- Paso 6: El choque de trenes
-- Acá está el problema visualizado. Comparamos lo que nos devolvió nuestro foldr incompleto, contra lo que nos exige la meta del Paso 1:

-- Lo que nos quedó: [a] -> Int -> [a] (Espera lista, luego número)

-- Lo que exige take: Int -> [a] -> [a] (Espera número, luego lista)

-- ¡Están cruzados! Tienen exactamente los mismos tipos, pero en el orden inverso. Si vos definís take = foldr ctk (\n -> [])

-- take' :: Int -> [a] -> [a]
-- take' = flip (foldr ctk (\n -> []))
--     where ctk x h = \n -> if n == 0 then [] else x : h (n-1)

take' :: Int -> [a] -> [a]
take' = flip (foldr (\x h n -> if n == 0 then [] else x : h (n-1)) (const []))