data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA

foldExpA :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExpA -> b
foldExpA fc fs fp (Cte n)          = fc n
foldExpA fc fs fp (Suma exp1 exp2) = fs (foldExpA fc fs fp exp1) (foldExpA fc fs fp exp2)
foldExpA fc fs fp (Prod exp1 exp2) = fp (foldExpA fc fs fp exp1) (foldExpA fc fs fp exp2)


-- i. que describe la cantidad de ceros explícitos en la expresión dada.
cantidadDeCeros :: ExpA -> Int
cantidadDeCeros = foldExpA contarSiEsCero (+) (+)

contarSiEsCero :: Int -> Int
contarSiEsCero 0 = 1
contarSiEsCero _ = 0

-- ii., que describe si la expresión dada no tiene números negativos de manera explícita.
noTieneNegativosExplicitosExpA :: ExpA -> Bool
noTieneNegativosExplicitosExpA = foldExpA (>=0) (&&) (&&)

-- iii. que describe una expresión con el mismo significado que la dada, pero que no tiene
-- sumas del número 0 ni multiplicaciones por 1 o por 0. La resolución debe ser exclusivamente simbólica.
simplificarExpA' :: ExpA -> ExpA
simplificarExpA' = foldExpA Cte simplificarSuma simplificarProd

simplificarSuma :: ExpA -> ExpA -> ExpA
simplificarSuma (Cte 0) e2 = e2
simplificarSuma e1 (Cte 0) = e1
simplificarSuma e1 e2      = Suma e1 e2

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd (Cte 0) e2 = e2
simplificarProd e1 (Cte 0) = e1
simplificarProd (Cte 1) e2 = e2
simplificarProd e1 (Cte 1) = e1
simplificarProd e1 e2      = Prod e1 e2

-- iv. , que describe el número que resulta
-- de evaluar la cuenta representada por la expresión aritmética dada.
evalExpA' :: ExpA -> Int
evalExpA' = foldExpA id (+) (*)

-- v. , que describe el string sin
-- espacios y con paréntesis correspondiente a la expresión dada.
showExpA :: ExpA -> String
showExpA = foldExpA show (mostrarOp "+") (mostrarOp "*")

mostrarOp :: String -> String -> String -> String
mostrarOp op e1 e2 = "(" ++ e1 ++ op ++ e2 ++ ")"


-- d
recExpA :: (Int -> b) -> (ExpA -> ExpA -> b -> b -> b) -> (ExpA -> ExpA -> b -> b -> b) -> ExpA -> b
recExpA fc fs fp (Cte n)          = fc n
recExpA fc fs fp (Suma exp1 exp2) = fs exp1 exp2 (recExpA fc fs fp exp1) (recExpA fc fs fp exp2)
recExpA fc fs fp (Prod exp1 exp2) = fp exp1 exp2 (recExpA fc fs fp exp1) (recExpA fc fs fp exp2)

cantDeSumaCeros :: ExpA -> Int
cantDeSumaCeros = recExpA (const 0) 
                        (\exp1 exp2 n1 n2 -> contarSiConstCero exp1 exp2 + n1 + n2)
                        (\exp1 exp2 n1 n2 -> n1 + n2)
                        

contarSiConstCero :: ExpA -> ExpA -> Int
contarSiConstCero (Cte 0) _ = 1
contarSiConstCero _ (Cte 0) = 1
contarSiConstCero _ _       = 0

cantDeProdUnos :: ExpA -> Int
cantDeProdUnos = recExpA (const 0) 
                        (\exp1 exp2 n1 n2 -> n1 + n2)
                        (\exp1 exp2 n1 n2 -> contarSiConstUno exp1 exp2 + n1 + n2)

contarSiConstUno :: ExpA -> ExpA -> Int
contarSiConstUno (Cte 1) _ = 1
contarSiConstUno _ (Cte 1) = 1
contarSiConstUno _ _       = 0

expresionDePrueba :: ExpA
expresionDePrueba = 
  Suma 
    (Suma (Cte 0) (Cte 5)) 
    (Prod 
      (Suma (Cte 8) (Cte 0)) 
      (Prod (Cte 0) (Cte 3))
    )

data EA = Const Int | BOp BinOp EA EA

data BinOp = Sum | Mul deriving Show

foldEA :: (Int -> b) -> (BinOp -> b -> b -> b) -> EA -> b
foldEA fc fb (Const n)         = fc n
foldEA fc fb (BOp binop e1 e2) = fb binop (foldEA fc fb e1) (foldEA fc fb e2)

noTieneNegativosExplicitosEA :: EA -> Bool
noTieneNegativosExplicitosEA = foldEA (>0) (const (&&))

simplificarEA' :: EA -> EA
simplificarEA' = foldEA Const (\binop e1 e2 -> case binop of 
                                                        Sum -> simplificarSum e1 e2
                                                        Mul -> simplificarMul e1 e2)

simplificarSum :: EA -> EA -> EA
simplificarSum (Const 0) e2 = e2
simplificarSum e1 (Const 0) = e1
simplificarSum e1 e2        = BOp Sum e1 e2

simplificarMul :: EA -> EA -> EA
simplificarMul (Const 0) e2 = e2
simplificarMul e1 (Const 0) = e1
simplificarMul (Const 1) e2 = e2
simplificarMul e1 (Const 1) = e1
simplificarMul e1 e2        = BOp Mul e1 e2 

evalEA' :: EA -> Int
evalEA' = foldEA id evaluar 

evaluar :: BinOp -> Int -> Int -> Int
evaluar Sum = (+)
evaluar Mul = (*)

showEA :: EA -> String
showEA = foldEA show (mostrarOp . show)

ea2ExpA' :: EA -> ExpA
ea2ExpA' = foldEA Cte armarExpA

armarExpA :: BinOp -> ExpA -> ExpA -> ExpA
armarExpA Sum = Suma
armarExpA Mul = Prod

data ABTree a b = Leaf b | Node a (ABTree a b) (ABTree a b) deriving (Show)

ea2Arbol' :: EA -> ABTree BinOp Int
ea2Arbol' = foldEA Leaf Node 

-- Ejercicio 3

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

foldT :: b -> (a -> b -> b -> b) -> Tree a -> b
foldT fe fn EmptyT          = fe
foldT fe fn (NodeT x t1 t2) = fn x (foldT fe fn t1) (foldT fe fn t2)

-- foldT ::    ((a -> b) -> Tree b) -> 
            -- (a -> ((a -> b) -> Tree b) -> ((a -> b) -> Tree b) -> (a -> b) -> Tree b) ->              
            -- Tree a ->
            -- ((a -> b) -> Tree b)

mapT :: (a -> b) -> Tree a -> Tree b
mapT = flip (foldT (const EmptyT) (\x t1 t2 f -> NodeT (f x) (t1 f) (t2 f)))

sumT :: Tree Int -> Int
sumT = foldT 0 (\n t1 t2 -> n + t1 + t2)

sizeT :: Tree a -> Int
sizeT = foldT 0 (\_ t1 t2 -> 1 + t1 + t2)

heightT :: Tree a -> Int
heightT = foldT 0 (\_ t1 t2 -> 1 + max t1 t2)

preOrder :: Tree a -> [a]
preOrder = foldT [] (\n t1 t2 -> n : t1 ++ t2 )

inOrder :: Tree a -> [a]
inOrder = foldT [] (\n t1 t2 -> t1 ++ [n] ++ t2)

postOrder :: Tree a -> [a]
postOrder = foldT [] (\n t1 t2 -> t1 ++ t2 ++ [n])

mirrorT :: Tree a -> Tree a
mirrorT = foldT EmptyT (flip . NodeT)

countByT :: (a -> Bool) -> Tree a -> Int
countByT = flip (foldT (const 0) (\n t1 t2 f -> unoSi (f n) + t1 f + t2 f))

unoSi :: Bool -> Int
unoSi True = 1
unoSi _    = 0

partitionT :: (a -> Bool) -> Tree a -> ([a], [a])
partitionT = undefined

zipWithT :: (a->b->c) -> Tree a -> Tree b -> Tree c
zipWithT = undefined

caminoMasLargo :: Tree a -> [a]
caminoMasLargo = undefined

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos = undefined

todosLosNiveles :: Tree a -> [[a]]
todosLosNiveles = undefined

nivelN :: Tree a -> Int -> [a]
nivelN = undefined















