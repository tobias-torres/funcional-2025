data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA

foldExpA :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExpA -> b
foldExpA c s p (Cte n)          = c n
foldExpA c s p (Suma exp1 exp2) = s (foldExpA c s p exp1) (foldExpA c s p exp2)
foldExpA c s p (Prod exp1 exp2) = p (foldExpA c s p exp1) (foldExpA c s p exp2)

cantidadDeCeros :: ExpA -> Int
cantidadDeCeros = foldExpA unoSiEsCero (+) (+)

unoSiEsCero :: Int -> Int
unoSiEsCero 0 = 1
unoSiEsCero _ = 0

noTieneNegativosExplicitosExpA :: ExpA -> Bool
noTieneNegativosExplicitosExpA = foldExpA (>0) (&&) (&&)

simplificarExpA' :: ExpA -> ExpA
simplificarExpA' = foldExpA Cte simplificarSuma simplificarProd

simplificarSuma :: ExpA -> ExpA -> ExpA
simplificarSuma (Cte 0) m = m
simplificarSuma n (Cte 0) = n
simplificarSuma n m       = Suma n m

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd (Cte 0) m = (Cte 0)
simplificarProd n (Cte 0) = (Cte 0)
simplificarProd (Cte 1) m = m
simplificarProd n (Cte 1) = n
simplificarProd n m       = Prod n m

evalExpA' :: ExpA -> Int
evalExpA' = foldExpA id (+) (*)

showExpA :: ExpA -> String
showExpA = foldExpA (\n -> "n") showSuma showProd

showSuma n m = "(" ++ n ++ " + " ++ m ++ ")"
showProd n m = "(" ++ n ++ " * " ++ m ++ ")"

recExpA :: (Int -> b) -> (ExpA -> ExpA -> b -> b -> b) -> (ExpA -> ExpA -> b -> b -> b) -> ExpA -> b
recExpA c s p (Cte n)          = c n
recExpA c s p (Suma exp1 exp2) = s exp1 exp2 (recExpA c s p exp1) (recExpA c s p exp2)
recExpA c s p (Prod exp1 exp2) = p exp1 exp2 (recExpA c s p exp1) (recExpA c s p exp2)

-- que describe la cantidad de constructores de suma con al menos uno de sus hijos constante cero
cantDeSumaCeros :: ExpA -> Int
cantDeSumaCeros = recExpA (const 0) (\exp1 exp2 n1 n2 -> cantDeSumaCeroSuma exp1 exp2 + n1 + n2 ) (\exp1 exp2 n1 n2 -> n1 + n2)

cantDeSumaCeroSuma :: ExpA -> ExpA -> Int
cantDeSumaCeroSuma (Cte 0) _ = 1
cantDeSumaCeroSuma _ (Cte 0) = 1
cantDeSumaCeroSuma _ _       = 0

cantDeProdUnos :: ExpA -> Int
cantDeProdUnos = recExpA (const 0) (\exp1 exp2 n1 n2 -> n1 + n2) (\exp1 exp2 n1 n2 -> cantDeProdUnoProd exp1 exp2 + n1 + n2)

cantDeProdUnoProd :: ExpA -> ExpA -> Int
cantDeProdUnoProd (Cte 1) _ = 1
cantDeProdUnoProd _ (Cte 1) = 1
cantDeProdUnoProd _ _       = 0

----------------------------------------------------------------------------------------------------------------

data EA = Const Int | BOp BinOp EA EA deriving(Show)

data BinOp = Sum | Mul deriving(Show)

data ABTree a b = Leaf b | Node a (ABTree a b) (ABTree a b) deriving Show

foldEA :: (Int -> b) -> (BinOp -> b -> b -> b) -> EA -> b
foldEA fc fb (Const n)         = fc n
foldEA fc fb (BOp binop e1 e2) = fb binop (foldEA fc fb e1) (foldEA fc fb e2)

noTieneNegativosExplicitosEA :: EA -> Bool
noTieneNegativosExplicitosEA = foldEA (>=0) (\binop e1 e2 -> e1 && e2) -- (const (&&))

simplificarEA' :: EA -> EA
simplificarEA' = foldEA Const (\binop e1 e2 -> evaluarSignificado binop e1 e2)
-- simplificarEA' = foldEA Const evaluarSignificado

evaluarSignificado :: BinOp -> EA -> EA -> EA
evaluarSignificado Sum n m = simplificarSumaEA n m
evaluarSignificado Mul n m = simplificarProdEA n m

simplificarSumaEA :: EA -> EA -> EA
simplificarSumaEA (Const 0) e = e
simplificarSumaEA e (Const 0) = e
simplificarSumaEA e1 e2       = BOp Sum e1 e2

simplificarProdEA :: EA -> EA -> EA
simplificarProdEA (Const 0) e = (Const 0)
simplificarProdEA e (Const 0) = (Const 0)
simplificarProdEA (Const 1) e = e
simplificarProdEA e (Const 1) = e
simplificarProdEA e1 e2       = BOp Mul e1 e2

evalEA' :: EA -> Int
evalEA' = foldEA id evaluarOperacion 

evaluarOperacion :: BinOp -> Int -> Int -> Int
evaluarOperacion Sum = (+) 
evaluarOperacion Mul = (*) 

ea2ExpA' :: EA -> ExpA
ea2ExpA' = foldEA Cte armarExpa

armarExpa :: BinOp -> ExpA -> ExpA -> ExpA
armarExpa Sum = Suma
armarExpa Mul = Prod

ea2Arbol' :: EA -> ABTree BinOp Int
ea2Arbol' = foldEA Leaf Node

ea1 = BOp Sum (BOp Mul (Const 1) (Const 2)) (BOp Sum (Const 4) (Const 0))

--------------------------------------------------------------------------------------------

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

foldT :: b -> (a -> b -> b -> b) -> Tree a -> b
foldT fe fn EmptyT          = fe
foldT fe fn (NodeT x t1 t2) = fn x (foldT fe fn t1) (foldT fe fn t2)

mapT :: (a -> b) -> Tree a -> Tree b
mapT f = foldT EmptyT (NodeT . f)

sumT :: Tree Int -> Int
sumT = foldT 0 (\x t1 t2 -> x + t1 + t2)

sizeT :: Tree a -> Int
sizeT = foldT 0 (\x t1 t2 -> 1 + t1 + t2)

heightT :: Tree a -> Int
heightT = foldT 0 (\x t1 t2 -> 1 + max t1 t2)

preOrder :: Tree a -> [a]
preOrder = foldT [] (\x t1 t2 -> [x] ++ t1 ++ t2)

inOrder :: Tree a -> [a]
inOrder = foldT [] (\x t1 t2 -> t1 ++ [x] ++ t2)

postOrder :: Tree a -> [a]
postOrder = foldT [] (\x t1 t2 -> t1 ++ t2 ++ [x])

mirrorT :: Tree a -> Tree a
mirrorT = foldT EmptyT (flip . NodeT)

countByT :: (a -> Bool) -> Tree a -> Int
countByT f = foldT 0 (\x t1 t2 -> if f x then 1 + t1 + t2 else t1 + t2)

partitionT :: (a -> Bool) -> Tree a -> ([a], [a])
partitionT f = foldT ([],[]) (\x (p1,p2) (p3,p4) -> if f x then (x : p1 ++ p3, p2 ++ p4 ) else ( p1 ++ p3, x : p2 ++ p4))

zipWithT' :: (a->b->c) -> Tree a -> Tree b -> Tree c
zipWithT' f EmptyT t2                         = EmptyT
zipWithT' f t1 EmptyT                         = EmptyT
zipWithT' f (NodeT x t1 t2) (NodeT y t1' t2') = NodeT (f x y) (zipWithT' f t1 t1') (zipWithT' f t2 t2')

zipWithT :: (a->b->c) -> Tree a -> Tree b -> Tree c
zipWithT f = foldT (const EmptyT) (\x t1 t2 t3 -> case t3 of 
                                EmptyT -> EmptyT
                                NodeT y ti td -> NodeT (f x y) (t1 ti)(t2 td))

zipWithTS :: (a->b->c) -> Tree a -> Tree b -> Tree c
zipWithTS f t1 t2 = foldT z g t1 t2
                        where z t2                              = EmptyT
                              g x arb1 arb2 EmptyT              = EmptyT
                              g x arb1 arb2 (NodeT y arb3 arb4) = NodeT (f x y) (arb1 arb3) (arb2 arb4) 

caminoMasLargoC :: Tree a -> [a] -- O(N^2) Esto es CARO 
caminoMasLargoC = foldT [] (\x xs ys -> if length xs > length ys then x : xs else x : ys)

caminoMasLargo :: Tree a -> [a] -- O(N)
caminoMasLargo = snd . foldT (0,[]) (\x (n,xs) (m,ys) -> if n >= m
                                                         then (n+1, x:xs)
                                                         else (m+1, x:ys))

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos = foldT [] (\x xss yss -> if null xss && null yss 
                                            then [[x]]
                                            else agregarRaiz x (xss ++ yss))

agregarRaiz :: a -> [[a]] -> [[a]]
agregarRaiz x []       = []
agregarRaiz x (xs:xss) = (x:xs) : agregarRaiz x xss

treeA :: Tree Int
treeA = (NodeT 1 (
                (NodeT 10) EmptyT EmptyT) 
                ((NodeT 22) EmptyT 
                ((NodeT 23) EmptyT EmptyT)))

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles [] yss            = yss
juntarNiveles xss []            = xss
juntarNiveles (xs:xss) (ys:yss) = (xs ++ ys) : (juntarNiveles xss yss)

todosLosNivelesC :: Tree a -> [[a]]
todosLosNivelesC = foldT [] (\x xss yss -> if null xss && null yss
                                            then [[x]]
                                            else [x] : juntarNiveles xss yss)

-- nivelN :: Tree a -> Int -> [a]
-- nivelN EmptyT n          = []
-- nivelN (NodeT x t1 t2) 0 = [x]
-- nivelN (NodeT x t1 t2) n = ((nivelN t1 (n-1)) ++ (nivelN t2 (n-1)))

nivelN :: Tree a -> Int -> [a]
nivelN = foldT (const []) (\x xs ys n -> if n == 0 then [x] else xs (n-1) ++ ys (n-1))

recT :: b -> (a -> Tree a -> Tree a -> b -> b -> b) -> Tree a -> b
recT fe fn EmptyT          = fe
recT fe fn (NodeT x t1 t2) = fn x t1 t2 (recT fe fn t1) (recT fe fn t2)

-- insertT :: a -> Tree a -> Tree a
-- insertT e = recT ()

-- ## Ejercicio 6 ##

data Dir = Izq | Der | Derecho deriving (Show)

data Mapa a = Cofre [a] | Nada (Mapa a) | Bifurcacion [a] (Mapa a) (Mapa a) deriving (Show)

foldM :: ([a] -> b) -> ( b -> b ) -> ([a] -> b -> b -> b) -> Mapa a -> b
foldM fc fn fb (Cofre xs)             = fc xs
foldM fc fn fb (Nada m)               = fn (foldM fc fn fb m)
foldM fc fn fb (Bifurcacion xs m1 m2) = fb xs (foldM fc fn fb m1) (foldM fc fn fb m2)

recM :: ([a] -> b) -> (Mapa a -> b -> b) -> ([a] -> Mapa a -> Mapa a -> b -> b -> b) -> Mapa a -> b
recM fc fn fb (Cofre xs)             = fc xs
recM fc fn fb (Nada m)               = fn m (recM fc fn fb m)
recM fc fn fb (Bifurcacion xs m1 m2) = fb xs m1 m2 (recM fc fn fb m1) (recM fc fn fb m2)

objects' :: Mapa a -> [a]
objects' = foldM id id (\xs m1 m2 -> xs ++ m1 ++ m2 )

mapM :: (a -> b) -> Mapa a -> Mapa b
mapM f = foldM (Cofre . map f ) Nada (Bifurcacion . map f )

has' :: (a-> Bool)-> Mapa a-> Bool
has' f = foldM (any f) id (\xs m1 m2 -> any f xs || m1 || m2)

-- hasObjectAt' :: (a -> Bool) -> Mapa a -> [Dir] -> Bool
-- hasObjectAt' f m []     = hayObjAcá f m
-- hasObjectAt' f m (d:ds) = case m of
--   (Cofre xs)              -> False
--   (Nada m')               -> esAdelante d && hasObjectAt' m' ds
--   (Bifurcacion xs m' m'') -> case d of
--     Izq -> hasObjectAt' ds m'
--     Der -> hasObjectAt' ds m''

-- hayObjAcá :: (a -> Bool) -> Mapa a -> Bool
-- hayObjAcá f (Cofre xs)           = any f xs
-- hayObjAcá f (Bifurcacion xs _ _) = any f xs
-- hayObjAcá _ _                    = False

hasObjectAt :: (a->Bool) -> Mapa a -> [Dir] -> Bool
hasObjectAt f = foldM (\xs ds -> null ds && any f xs)
                      (\b ds -> case ds of
                        (Derecho:ds') -> b ds'
                        _              -> False)
                      (\xs b b' ds -> case ds of
                        []        -> any f xs
                        (Izq:ds') -> b  ds'
                        (Der:ds') -> b' ds'
                        _         -> False)

-- longestPath' :: Mapa a -> [Dir]
-- longestPath' (Cofre xs)             = []
-- longestPath' (Nada m)               = Derecho : longestPath' m
-- longestPath' (Bifurcacion xs m1 m2) = if length (longestPath' m1) > length (longestPath' m2)
--                                         then Izq : longestPath' m1
--                                         else Der : longestPath' m2

longestPath' :: Mapa a -> [Dir] -- Carisimo
longestPath' = foldM (const []) (\m -> Derecho : m) (\xs d1 d2 -> if length d1 > length d2 
                                                                then Izq : d1
                                                                else Der : d2)

longestPath :: Mapa a -> [Dir] 
longestPath = snd . foldM (\xs -> (0,[]))
                    (\(n,m) -> (1+n, Derecho : m))
                    (\xs (n,d1) (m,d2) -> if n >=m
                                          then (n+1, Izq : d1)
                                          else (m+1, Der : d2))

objectsOfLongestPath :: Mapa a -> [a] 
objectsOfLongestPath = snd . foldM (\xs -> (0,xs)) 
                             (\(n,m) -> (1+n, m))
                             (\xs (n,m1) (m,m2) -> if n>=m 
                                                    then (n+1, xs ++ m1)
                                                    else (m+1, xs ++ m2))

allPaths :: Mapa a -> [[Dir]]
allPaths = foldM (const [[]] )
                 (agregarRaiz Derecho)
                 (\xs d1 d2 -> (agregarRaiz Izq d1 ++ agregarRaiz Der d2) )

objectsPerLevel' :: Mapa a -> [[a]] 
objectsPerLevel' (Cofre xs)             = [xs]
objectsPerLevel' (Nada m)               = objectsPerLevel' m
objectsPerLevel' (Bifurcacion xs m1 m2) = [xs] ++ juntarNiveles (objectsPerLevel' m1) (objectsPerLevel' m2)

objectsPerLevel :: Mapa a -> [[a]]
objectsPerLevel = foldM (\xs -> [xs]) id (\xs ls ls' -> [xs] ++ juntarNiveles ls ls')




map1 = Bifurcacion [1,2] (Cofre [3,4]) (Cofre [5,5])

map3 = Nada $ Bifurcacion [1,2] (Cofre [9,0])
                                (Bifurcacion [3,4] (Cofre [5,6]) (Nada $ Cofre [7,8]))