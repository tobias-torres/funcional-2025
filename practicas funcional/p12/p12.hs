import Prelude hiding (Left, Right, Straight)

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

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving (Show)

foldT :: b -> (a -> b -> b -> b) -> Tree a -> b
foldT fe fn EmptyT          = fe
foldT fe fn (NodeT x t1 t2) = fn x (foldT fe fn t1) (foldT fe fn t2)

-- foldT ::    ((a -> b) -> Tree b) -> 
            -- (a -> ((a -> b) -> Tree b) -> ((a -> b) -> Tree b) -> (a -> b) -> Tree b) ->              
            -- Tree a ->
            -- ((a -> b) -> Tree b)

-- mapT :: (a -> b) -> Tree a -> Tree b
-- mapT = flip (foldT (const EmptyT) (\x t1 t2 f -> NodeT (f x) (t1 f) (t2 f)))

-- mapT :: (a -> b) -> Tree a -> Tree b
-- mapT f t = foldT EmptyT (\x t1 t2 -> NodeT (f x) t1 t2 )

sumT :: Tree Int -> Int
sumT = foldT 0 (\n t1 t2 -> n + t1 + t2)
              --  ((+) .) . (+)

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
countByT f = foldT 0 (\n t1 t2 -> unoSi (f n) + t1 + t2)
-- countByT = flip (foldT (const 0) (\n t1 t2 f -> unoSi (f n) + t1 f + t2 f))

unoSi :: Bool -> Int
unoSi True = 1
unoSi _    = 0

partition :: (a -> Bool) -> [a] -> ([a], [a]) 
partition p = foldr (\x par -> if p x then (x : fst par, snd par) else (fst par, x : snd par)) ([],[])

-- partitionT p = foldT ([],[]) (\x t1 t2 -> if p x then (x : t1, t2) else (t1, x : t2)) 
-- partitionT :: (a -> Bool) -> Tree a -> ([a], [a])
-- partitionT f EmptyT          = ([],[])
-- partitionT f (NodeT x t1 t2) = if f x then (x : cumplenIzq ++ cumplenDer, noCumplenIzq ++ noCumplenDer) 
--                                       else (cumplenIzq ++ cumplenDer, x : noCumplenIzq ++ noCumplenDer)
--                                   where
--                                     (cumplenIzq, noCumplenIzq) = partitionT f t1
--                                     (cumplenDer, noCumplenDer) = partitionT f t2
-- partitionT f (NodeT x t1 t2) = if f x then (x : fst (partitionT f t1) ++ snd (partitionT f t2), fst (partitionT f t1) ++ snd (partitionT f t2))
--                                       else (fst (partitionT f t1) ++ snd (partitionT f t2), x : fst (partitionT f t1) ++ snd (partitionT f t2))

partitionT :: (a -> Bool) -> Tree a -> ([a],[a])
partitionT f = foldT ([],[]) (\n t1 t2 -> if f n then (n : fst t1 ++ snd t2, fst t1 ++ snd t2)
                                                 else (fst t1 ++ snd t2, n : fst t1 ++ snd t2))


-- zipWithT :: (a->b->c) -> Tree a -> Tree b -> Tree c
-- zipWithT f EmptyT t                        = EmptyT
-- zipWithT f t EmptyT                        = EmptyT
-- zipWithT f (NodeT x t1 t2) (NodeT y t3 t4) = NodeT (f x y) (zipWithT f t1 t3) (zipWithT f t2 t4) 

zipWithT :: (a->b->c) -> Tree a -> Tree b -> Tree c
zipWithT f = foldT (const EmptyT) (\n t1 t2 t3 -> case t3 of
                                      EmptyT           -> EmptyT
                                      (NodeT x t3' t4) -> NodeT (f n x) (t1 t3') (t2 t4))

-- caminoMasLargo :: Tree a -> [a]
-- caminoMasLargo = foldT [] (\x xs ys -> if length xs > length ys
                                        -- then x : xs
                                        -- else x : ys)
caminoMasLargo :: Tree a -> [a]
caminoMasLargo = snd . foldT (0, []) (\x (n,xs) (m,ys) -> if n >= m
                                                    then (n+1, x :xs)
                                                    else (m+1, x : ys))

t :: Tree Int
t = 
    NodeT 1 
        (NodeT 2 
            (NodeT 4 EmptyT EmptyT) 
            (NodeT 5 EmptyT EmptyT)
        ) 
        (NodeT 3 
            (NodeT 6 EmptyT EmptyT) 
            (NodeT 7 
                (NodeT 8 (NodeT 33 EmptyT (NodeT 38 EmptyT EmptyT)) EmptyT) 
                (NodeT 9 (NodeT 10 EmptyT EmptyT) EmptyT)
            )
        )

-- todosLosCaminos :: Tree a -> [[a]]
-- todosLosCaminos EmptyT                  = []
-- todosLosCaminos (NodeT x EmptyT EmptyT) = [[x]]
-- todosLosCaminos (NodeT x t1 t2)         = agregarRaiz x (todosLosCaminos t1 ++ todosLosCaminos t2)

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos = foldT [] (\x xss yss -> if null xss && null yss
                                            then [[x]]
                                            else agregarRaiz x (xss ++ yss))

agregarRaiz :: a -> [[a]] -> [[a]]
agregarRaiz x [] = []
agregarRaiz x (xs:xss) = (x : xs) : agregarRaiz x xss

todosLosNiveles :: Tree a -> [[a]]
todosLosNiveles = foldT [] (\x xss yss -> [x] : concatPerLevel xss yss)

-- todosLosNiveles :: Tree a -> [[a]]
-- todosLosNiveles EmptyT          = []
-- todosLosNiveles (NodeT x t1 t2) = [x] : concatPerLevel (todosLosNiveles t1) (todosLosNiveles t2)

concatPerLevel :: [[a]] -> [[a]] -> [[a]]
concatPerLevel [] yss           = yss
concatPerLevel xss []           = xss
concatPerLevel (xs:xss)(ys:yss) = (xs ++ ys) : concatPerLevel xss yss

nivelN :: Tree a -> Int -> [a]
nivelN = foldT (const []) (\x t1 t2 n -> case n of 
                                  0 -> [x]
                                  n -> t1 (n-1) ++ t2 (n-1))
-- nivelN = foldT (const []) (\x t1 t2 n -> if n == 0 then [x] else t1 (n-1) ++ t2 (n-1))

-- c

recT :: b -> (a -> Tree a -> Tree a -> b -> b -> b) -> Tree a -> b
recT fe fn EmptyT          = fe
recT fe fn (NodeT x t1 t2) = fn x t1 t2 (recT fe fn t1) (recT fe fn t2)

insertT :: Ord a => a -> Tree a -> Tree a
-- insertT e = recT (NodeT e EmptyT EmptyT) (\x t1 t2 t t' -> if e < x then NodeT x t t2 else NodeT x t1 t')
insertT = flip (recT (\e -> NodeT e EmptyT EmptyT) (\x t1 t2 t t' e -> if e < x then NodeT x (t e) t2 else NodeT x t1 (t' e)))

-- arbol1 :: Tree Int
-- arbol1 = NodeT 8 
--               (NodeT 6 (NodeT 4 
--                               (NodeT 2 EmptyT EmptyT)
--                                EmptyT)
-- 							         (NodeT 7 EmptyT EmptyT))
-- 				      (NodeT 10 (NodeT 9 EmptyT EmptyT)
-- 							          (NodeT 11 EmptyT EmptyT))

caminoHasta :: Eq a => a -> Tree a -> [a]
caminoHasta e = (foldT [] (\x t t' -> if x == e then [x] else if null (t ++ t') then [] else x : (t ++ t')))
-- caminoHasta = flip (foldT (\e -> []) (\x t t' e -> if x == e then [x] else if null ((t e) ++ (t' e)) then [] else x : ((t e)++ (t' e))))

-- caminoHasta e EmptyT          = []
-- caminoHasta e (NodeT x t1 t2) = if x == e 
--                                     then [x]
--                                     else if null resultadoHijos
--                                             then []
--                                             else x : resultadoHijos
--                                         where resultadoHijos = caminoHasta e t1 ++ caminoHasta e t2

-- i.
-- para todo f. sizeT . mapT f = sizeT
-- para todo f. para todo t. (sizeT . mapT f) t = sizeT t ?
-- por def de (.)
-- para todo f. para todo t. sizeT (mapT f t) = sizeT t ?
-- sea g una funcion, t' un tipo :: Tree a, por ppio de induccion sobre la estructura de t', es equivalente demostrar que:

-- Caso Base: t' = EmptyT

-- sizeT (mapT g EmptyT) = sizeT EmptyT ?

-- Caso Induccion: t' = (NodeT x t1 t2)

-- HI1) sizeT (mapT g t1) = sizeT t1 !
-- HI2) sizeT (mapT g t2) = sizeT t2 !
-- TI) sizeT (mapT g (NodeT x t1 t2)) = sizeT (NodeT x t1 t2) ?

-- Caso Base:

-- I)

-- sizeT (mapT g EmptyT)
--        -------------
-- =                   
-- sizeT EmptyT

-- D)

-- sizeT EmptyT

-- Caso Inductivo:

-- I)

-- sizeT (mapT g (NodeT x t1 t2))
--        ----------------------
-- =                       def de mapT
-- sizeT (NodeT (g x) (mapT g t1) (mapT g t2))
-- -------------------------------------------
-- =                       def de sizeT
-- 1 + sizeT (mapT g t1) + sizeT (mapT g t2)
--     ----------------    -----------------
-- =                       HI1, HI2
-- 1 + sizeT t1 + sizeT t2

-- D)

-- sizeT (NodeT x t1 t2)
-- ---------------------
-- =                       def de sizeT
-- 1 + sizeT t1 + sizeT t2


-- ii. para todo f. para todo g. mapT f . mapT g = mapT (f . g) ?
-- para todo f. para todo g. para todo t. (mapT f . mapT g) t = mapT (f . g) t ?
-- por def de (.)
-- para todo f. para todo g. para todo t. mapT f (mapT g t) = mapT f (g t) ? 
-- sea f' y g' dos funciones y t' de tipo :: Tree a, por ppio de induccion sobre la estructura de t', es equivalente demostrar que:

-- Caso Base: t' = EmptyT

-- mapT f' (mapT g' EmptyT) = mapT f' (g' EmptyT) ?

-- Caso Inductivo: t' = (NodeT x t1 t2)

-- HI1) mapT f' (mapT g' t1) = mapT f' (g' t1) !
-- HI2) mapT f' (mapT g' t2) = mapT f' (g' t2) !
-- TI) mapT f' (mapT g' (NodeT x t1 t2)) = mapT f' (g' (NodeT x t1 t2)) ?

-- Caso Base:

-- I)

-- mapT f' (mapT g' EmptyT)
--         ---------------
-- =                       def de mapT
-- mapT f' EmptyT

-- D)

-- mapT f' (g' EmptyT)


-- Ejercicio 4

data Dir = Left | Right | Straight deriving (Show)

data Mapa a = Cofre [a]  
            | Nada (Mapa a)  
            | Bifurcacion [a] (Mapa a) (Mapa a)

-- a

foldM :: ([a] -> b) -> (b -> b) -> ([a] -> b -> b -> b) -> Mapa a -> b
foldM fc fn fb (Cofre xs)             = fc xs
foldM fc fn fb (Nada m)               = fn (foldM fc fn fb m)
foldM fc fn fb (Bifurcacion xs m1 m2) = fb xs (foldM fc fn fb m1) (foldM fc fn fb m2)

recM :: ([a] -> b) -> (Mapa a -> b -> b) -> ([a] -> Mapa a -> Mapa a -> b -> b -> b) -> Mapa a -> b
recM fc fn fb (Cofre xs)             = fc xs
recM fc fn fb (Nada m)               = fn m (recM fc fn fb m)
recM fc fn fb (Bifurcacion xs m1 m2) = fb xs m1 m2 (recM fc fn fb m1) (recM fc fn fb m2)

-- i. , que describe la lista de todos los objetos presentes en el mapa dado.
objects  :: Mapa a -> [a]
objects = foldM id id (\xs m1 m2 -> xs ++ m1 ++ m2)
                    -- ((++) .) . (++)

-- ii. , que transforma los objetos del mapa dado aplicando la función dada.
mapM :: (a -> b) -> Mapa a -> Mapa b
-- mapM f = foldM (\xs -> Cofre (map f xs)) (\m -> m ) (\xs m1 m2 -> Bifurcacion (map f xs) m1 m2)
mapM f = foldM (Cofre . map f) id ( Bifurcacion . map f)

-- iii. , que indica si existe algún objeto que cumpla con la condición dada en el mapa dado.
has :: (a -> Bool) -> Mapa a -> Bool
-- has f = foldM (\xs -> any f xs) id (\xs m1 m2 -> any f xs || m1 || m2 )
has f = foldM (any f) id (\xs m1 m2 -> any f xs || m1 || m2)

-- iv. , que indica si un objeto al final del camino dado cumple con la condición dada en el mapa dado. 
hasObjectAt :: (a -> Bool) -> Mapa a -> [Dir] -> Bool
hasObjectAt f = foldM (\xs ds -> null ds && any f xs) 
                      (\m ds -> case ds of
                                    Straight:ds' -> m ds'
                                    _           -> False)
                      (\xs m1 m2 ds -> case ds of 
                                        Left:ds'  -> m1 ds'
                                        Right:ds' -> m2 ds')


mapa = Bifurcacion [3] 
                    (Nada (Cofre [2,3,4]))
                    (Bifurcacion [55] 
                                    (Nada (Cofre [5,6,7])) 
                                    (Nada 
                                        (Bifurcacion [8,9] 
                                                          (Nada (Cofre []))
                                                          (Cofre []))))

-- longestPath :: Mapa a -> [Dir]
-- longestPath = foldM (\xs -> [])
--                     (\m -> Straight:m )
--                     (\xs m1 m2 -> if length m1 > length m2
--                                  then Left : m1
--                                  else Right : m2)

longestPath :: Mapa a -> [Dir]
longestPath = snd . foldM (\xs -> (0, []))
                          (\(n,m) -> (n+1, Straight:m ))
                          (\xs (n,m1) (m,m2) -> if n > m
                                                    then (n+1, Left : m1)
                                                    else (m+1, Right : m2))

objectsOfLongestPath :: Mapa a -> [a]
objectsOfLongestPath = snd . foldM (\xs -> (0,xs))
                                   (\(n,m) -> (n+1, m))
                                   (\xs (n,m1) (m,m2) -> if n > m
                                                    then (n+1, xs ++ m1)
                                                    else (m+1, xs ++ m2))

allPaths :: Mapa a -> [[Dir]]
allPaths = foldM (\xs -> [[]])
                 (\dirs  -> map (Straight :) dirs)
                 (\xs dirs1 dirs2 -> map (Left :) dirs1 ++ map (Right :) dirs2)

-- allPaths :: Mapa a -> [[Dir]]
-- allPaths (Cofre xs)             = [[]]
-- allPaths (Nada m)               = map (Straight :) (allPaths m)
-- allPaths (Bifurcacion xs m1 m2) = map (Left :) (allPaths m1) ++ map (Right :) (allPaths m2)

-- , que describe la lista con todos los objetos por niveles del mapa dado.
objectsPerLevel  ::  Mapa a -> [[a]] 
objectsPerLevel = foldM (\xs -> [xs])
                        (\objs -> []:objs)
                        (\xs objs1 objs2 -> xs : concatPerLevel objs1 objs2)


-- todosLosNiveles :: Tree a -> [[a]]
-- todosLosNiveles EmptyT          = []
-- todosLosNiveles (NodeT x t1 t2) = [x] : concatPerLevel (todosLosNiveles t1) (todosLosNiveles t2)

-- concatPerLevel :: [[a]] -> [[a]] -> [[a]]
-- concatPerLevel [] yss           = yss
-- concatPerLevel xss []           = xss
-- concatPerLevel (xs:xss)(ys:yss) = (xs ++ ys) : concatPerLevel xss yss

-- Ejercicio 7) 

type Record f v = [(f,v)] 

-- donde la idea de este tipo es representar una fila de una base de datos (el valor de 
-- tipo f es el nombre del campo, y el valor de tipo v es el valor de ese campo). Por 
-- ejemplo, para representar una fila de una base de datos de personas, se puede usar 
-- el siguiente dato:  

-- [("nombre","Grace"),("apellido","H"),("edad", "42"),("género","F"),("salario","120")] :: Record String String

type Table f v = [ Record f v ]

str2num :: String -> Int
str2num = read

personas =  
            [ 
              [("nombre","Graciela"),("apellido","H") 
              ,("edad", "42"),("genero","F"),("salario","120")] 
            , [("nombre","Alonso"),("apellido","Ch") 
              ,("edad", "35"),("genero","M"),("salario","250")] 
            , [("nombre","Adela"),("apellido","G") 
              ,("edad", "39"),("genero","F"),("salario","130")] 
            , [("nombre","Fer"),("apellido","R") 
              ,("edad", "22"),("genero","X"),("salario","100")] 
            , [("nombre","Felipe"),("apellido","W") 
              ,("edad", "50"),("genero","M"),("salario","280")] 
            ]

-- a. 
-- que a partir de la lista de registros dada describe la lista de los registros que cumplen con la condición dada. 
-- Por ejemplo, 
        
--         select (\r -> elem ("género", "F") r) 

-- selecciona todos los registros de personas de género femenino. 
-- Otro ejemplo es
        
--         select (\r -> any (\(c,v) -> c=="salario" && str2num v < 200) r)

-- que selecciona todos los registros de personas con salarios por debajo de 200.

select :: (Record f v -> Bool) -> Table f v -> Table f v
-- select f = (\r -> filter f r)
-- select f = (filter f)
select = filter

-- b.
-- que a partir de la lista de registros dada describe la lista de registros solo con los campos que cumplen la condición dada. 
-- Por ejemplo, 

        -- project (\f -> f=="nombre" || f=="apellido") 

-- selecciona los campos nombre y apellido de un tabla. 

project :: (f -> Bool) -> Table f v -> Table f v
-- project p = map (filter (\cv -> p (fst cv)))
-- project p = map (filter (p . fst))
project = map . filter . (. fst)

-- c.
-- , que describe el predicado que da True solo cuando los dos predicados dados lo hacen. 
-- Por ejemplo, 
   
      -- conjunt (=="nombre") (=="apellido") 

-- es otra forma de escribir la condición del ejemplo anterior. 

conjunct :: (a -> Bool) -> (a -> Bool) -> a -> Bool
conjunct p1 p2 e = p1 e && p2 e


-- d.  
-- que describe el resultado de aplicar una función a cada elemento del producto cartesiano 
-- de las dos listas dadas. 
    -- Por ejemplo 
    
          -- crossWith (+) [1,2,3] [10,20] 
    
    -- da como resultado [11,21,12,22,13,23].

crossWith :: (a -> b -> c) -> [a] -> [b] -> [c]
crossWith f xs ys = concat (map (\x -> map (f x) ys) xs)
-- crossWith f xs ys = concatMap (\x -> map (f x) ys ) xs

-- e.

-- , que describe la lista de registros resultante del producto cartesiano combinado de las dos 
-- listas de registros dadas. Es decir, la unión de los campos en los registros del 
-- producto cartesiano. 

product' :: Table f v -> Table f v -> Table f v
product' = crossWith (++)

r1 = [[("apellido","W"),("idMat","42")], [("apellido","G"),("idMat","17")]]
        
r2 = [[("idMat", "17"),("nomMat","PO")], [("idMat","666"),("nomMat","PI")],
        [("idMat", "42"),("nomMat","PF")]] 

r3 = [("apellido","W"), ("idMat","42"), ("idMat","42"), ("nombreMat","PF")]

-- que describe el registro 
-- resultante de descartar datos cuyos campos sean iguales (o sea, el mismo 
-- dato asociado al mismo campo). 
-- Por ejemplo,  
-- similar 
--     [("apellido","W"),("idMat","42")    ,("idMat","42"),("nombreMat","PF")] 
-- da como resultado 
--  [("apellido","W"),("idMat","42")    ,("nombreMat","PF")]

similar :: (Eq f, Eq v) => Record f v -> Record f v
similar = sinRepetidos

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos = foldr (\x xs -> if elem x xs then xs else x: xs) []