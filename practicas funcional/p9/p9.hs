data EA = Const Int | BOp BinOp EA EA deriving (Show)

data BinOp = Sum | Mul deriving (Show)

data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA

-- que describe el número que resulta de evaluar la cuenta representada por la expresión aritmética dada.
evalEA :: EA -> Int
evalEA (Const n)         = n
evalEA (BOp binop e1 e2) = evaluar binop (evalEA e1) (evalEA e2)

evaluar :: BinOp -> Int -> Int -> Int
evaluar Sum n m = n + m
evaluar Mul n m = n * m

-- que describe una expresión aritmética representada con el tipo ExpA, cuya estructura y significado son los mismos que la dada.
ea2ExpA :: EA -> ExpA
ea2ExpA (Const n)         = Cte n
ea2ExpA (BOp binop e1 e2) = armarExpA binop (ea2ExpA e1) (ea2ExpA e2) 

armarExpA :: BinOp -> ExpA -> ExpA -> ExpA
armarExpA Sum e1 e2 = Suma e1 e2
armarExpA Mul e1 e2 = Prod e1 e2

-- que describe una expresión aritmética representada con el tipo EA, cuya estructura y significado son los mismos que la dada.
expA2ea :: ExpA -> EA
expA2ea (Cte n)      = Const n
expA2ea (Suma e1 e2) = BOp Sum (expA2ea e1) (expA2ea e2)
expA2ea (Prod e1 e2) = BOp Mul (expA2ea e1) (expA2ea e2)

evalExpA :: ExpA -> Int
evalExpA (Cte n)      = n
evalExpA (Suma e1 e2) = evalExpA e1 + evalExpA e2
evalExpA (Prod e1 e2) = evalExpA e1 * evalExpA e2

----------------------------------------------------------------------------------------------------------------------------------------------

data Arbol a b = Hoja b | Nodo a (Arbol a b) (Arbol a b)

cantidadDeHojas :: Arbol a b -> Int
cantidadDeHojas (Hoja y)       = 1
cantidadDeHojas (Nodo x t1 t2) = cantidadDeHojas t1 + cantidadDeHojas t2


cantidadDeNodos :: Arbol a b -> Int
cantidadDeNodos (Hoja y)       = 0
cantidadDeNodos (Nodo x t1 t2) = 1 + cantidadDeNodos t1 + cantidadDeNodos t2

cantidadDeConstructores :: Arbol a b -> Int
cantidadDeConstructores (Hoja y)       = 1
cantidadDeConstructores (Nodo x t1 t2) = 1 + cantidadDeConstructores t1 + cantidadDeConstructores t2

ea2Arbol :: EA -> Arbol BinOp Int
ea2Arbol (Const n)         = Hoja n
ea2Arbol (BOp binop e1 e2) = Nodo binop (ea2Arbol e1) (ea2Arbol e2)

----------------------------------------------------------------------------------------------------------------------------------------------

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving (Show)

t :: Tree Int
t = NodeT 10 
        (NodeT 20 EmptyT EmptyT)
            (NodeT 40 EmptyT 
            (NodeT 33 EmptyT EmptyT))

miArbolDibujado :: Tree Int
miArbolDibujado = 
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

sumarT :: Tree Int -> Int
sumarT EmptyT          = 0
sumarT (NodeT x t1 t2) = x + sumarT t1 + sumarT t2

-- ii. sizeT :: Tree a -> Int, que describe la cantidad de elementos
-- en el árbol dado.
sizeT :: Tree Int -> Int
sizeT EmptyT          = 0
sizeT (NodeT x t1 t2) = 1 + sizeT t1 + sizeT t2

-- -- iii. anyT :: (a -> Bool) -> Tree a -> Bool, que indica si en el
-- -- árbol dado hay al menos un elemento que cumple con el predicado
-- -- dado.
anyT :: (a -> Bool) -> Tree a -> Bool
anyT f EmptyT          = False
anyT f (NodeT x t1 t2) = f x || anyT f t1 || anyT f t2

-- -- iv. countT :: (a -> Bool) -> Tree a -> Int, que describe la
-- -- cantidad de elementos en el árbol dado que cumplen con el predicado
-- -- dado.
countT :: (a -> Bool) -> Tree a -> Int
countT f EmptyT          = 0
countT f (NodeT x t1 t2) = if f x then 1 + countT f t1 + countT f t2 else countT f t1 + countT f t2

-- -- v. countLeaves :: Tree a -> Int, que describe la cantidad de
-- -- hojas del árbol dado.
countLeaves :: Tree a -> Int
countLeaves EmptyT          = 1
countLeaves (NodeT x t1 t2) = countLeaves t1 + countLeaves t2

-- -- vi. heightT :: Tree a -> Int, que describe la altura del árbol
-- -- dado.
heightT :: Tree Int -> Int
heightT EmptyT          = 0
heightT (NodeT x t1 t2) = 1 + max (heightT t1) (heightT t2)

-- -- vii. inOrder :: Tree a -> [a], que describe la lista in order con los
-- -- elementos del árbol dado.
inOrder :: Tree a -> [a]
inOrder EmptyT          = []
inOrder (NodeT x t1 t2) = inOrder t1 ++ [x] ++ inOrder t2

-- -- viii. listPerLevel :: Tree a -> [[a]], que describe la lista donde
-- -- cada elemento es una lista con los elementos de un nivel del árbol
-- -- dado.
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT x t1 t2) = [x] : concatPerLevel (listPerLevel t1) (listPerLevel t2)

concatPerLevel :: [[a]] -> [[a]] -> [[a]]
concatPerLevel [] yss           = yss
concatPerLevel xss []           = xss
concatPerLevel (xs:xss)(ys:yss) = (xs ++ ys) : concatPerLevel xss yss


-- -- ix. mirrorT :: Tree a -> Tree a, que describe un árbol con los
-- -- mismos elemento que el árbol dado pero en orden inverso.
mirrorT :: Tree a -> Tree a
mirrorT EmptyT          = EmptyT
mirrorT (NodeT x t1 t2) = NodeT x (mirrorT t2) (mirrorT t1)

-- -- x. levelN :: Int -> Tree a -> [a], que describe la lista con los
-- -- elementos del nivel dado en el árbol dado.
levelN :: Int -> Tree a -> [a]
levelN n EmptyT          = []
levelN 0 (NodeT x t1 t2) = [x]
levelN n (NodeT x t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2

-- -- xi. ramaMasLarga :: Tree a -> [a], que describe la lista con los
-- -- elementos de la rama más larga del árbol.
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT x t1 t2) = if length (ramaMasLarga t1) > length (ramaMasLarga t2) then x : ramaMasLarga t1 else x : ramaMasLarga t2

-- -- xii. todosLosCaminos :: Tree a -> [[a]], que describe la lista
-- -- con todos los caminos existentes en el árbol dado.
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT                  = []
todosLosCaminos (NodeT x EmptyT EmptyT) = [[x]]
todosLosCaminos (NodeT x t1 t2)         = agregarRaiz x (todosLosCaminos t1 ++ todosLosCaminos t2)

agregarRaiz :: a -> [[a]] -> [[a]]
agregarRaiz x [] = []
agregarRaiz x (xs:xss) = (x : xs) : agregarRaiz x xss

----------------------------------------------------------------------------------------------------------------------------------------------

data AppList a = Single a | Append (AppList a) (AppList a) deriving (Show)

ejemplo = Append (Single 2) (Append (Single 3) (Single 30))

ejemplo3 = Append (Append (Single 1) (Single 2)) (Single 3)

ejemplo2 = Append ((Append (Single 32) (Single 33))) (Single 34)

-- , que describe la cantidad de elementos de la lista.
lenAL :: AppList a -> Int
lenAL (Single x)       = 1
lenAL (Append ap1 ap2) = lenAL ap1 + lenAL ap2

-- ii. , que describe la lista resultante de agregar el elemento dado al principio de la lista dada.
consAL :: a -> AppList a -> AppList a
consAL e (Single x)       = Append (Single e) (Single x)
consAL e (Append ap1 ap2) = Append (consAL e ap1) ap2


-- iii. , que describe el primer elemento de la lista dada.
headAL :: AppList a -> a
headAL (Single x)       = x
headAL (Append ap1 ap2) = headAL ap1

-- iv. , que describe la lista
-- resultante de quitar el primer elemento de la lista dada.
tailAL :: AppList a -> AppList a
tailAL (Single x)              = error "Lista vacia"
tailAL (Append (Single x) ap2) = ap2
tailAL (Append ap1 ap2)        = Append (tailAL ap1) ap2

-- v., que describe la lista resultante de agregar el elemento dado al final de la lista dada.
snocAL :: AppList a -> a -> AppList a
snocAL (Single x)       e = Append (Single x) (Single e)
snocAL (Append ap1 ap2) e = Append ap1 (snocAL ap2 e)

-- snocAL :: AppList a -> a -> AppList a -- Mas Eficiente
-- snocAL xs e = Append xs (Single e)

-- vi. , que describe el último elemento de la lista dada.
lastAL :: AppList a -> a
lastAL (Single x)       = x
lastAL (Append ap1 ap2) = lastAL ap2

-- vii. , que describe la lista dada sin su último elemento.
initAL :: AppList a -> AppList a
initAL (Single x)              = error "Lista Vacia"
initAL (Append ap1 (Single x)) = ap1
initAL (Append ap1 ap2)        = Append ap1 (initAL ap2)

-- viii. , que describe la lista dada con sus elementos en orden inverso.
reverseAL :: AppList a -> AppList a
reverseAL (Single x)       = (Single x)
reverseAL (Append ap1 ap2) = Append (reverseAL ap2) (reverseAL ap1)

-- ix. , que indica si el elemento dado se encuentra en la lista dada.
elemAL :: Eq a => a -> AppList a -> Bool
elemAL e (Single x)       = e == x
elemAL e (Append ap1 ap2) = (elemAL e ap1) || (elemAL e ap2)


-- x. appendAL :: AppList a -> AppList a -> AppList a, que
-- describe el resultado de agregar los elementos de la primera lista
-- adelante de los elementos de la segunda.
-- NOTA: buscar la manera más eficiente de hacerlo.
appendAL :: AppList a -> AppList a -> AppList a
appendAL = Append

-- xi., que describe la
-- representación lineal de la lista dada
appListToList :: AppList a -> [a]
appListToList (Single x)       = [x]
appListToList (Append ap1 ap2) = appListToList ap1 ++ appListToList ap2

--------------------------------------------------------------------------------------------------------------------------------

data QuadTree a = LeafQ a | NodeQ (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a)

data Color = RGB Int Int Int

type Image = QuadTree Color

-- i. , que describe la altura del árbol dado.
heightQT :: QuadTree a -> Int
heightQT (LeafQ x)           = 0
heightQT (NodeQ q1 q2 q3 q4) = 1 + max (heightQT q1) (max (heightQT q2) (max (heightQT q3) (heightQT q4)))

-- ii. , que describe la cantidad de hojas del árbol dado.
countLeavesQT :: QuadTree a -> Int
countLeavesQT (LeafQ x)           = 1
countLeavesQT (NodeQ q1 q2 q3 q4) = (heightQT q1) + (heightQT q2) + (heightQT q3) + (heightQT q4)

-- iii. , que describe la cantidad de constructores del árbol dado.
sizeQT :: QuadTree a -> Int
sizeQT (LeafQ x)           = 1
sizeQT (NodeQ q1 q2 q3 q4) = 1 + (heightQT q1) + (heightQT q2) + (heightQT q3) + (heightQT q4)

-- iv. , que describe el árbol resultante de transformar en hoja todos aquellos nodos para los que se cumpla que todas sus hojas tengan el mismo valor.
compress :: QuadTree a -> QuadTree a
compress (LeafQ x)           = LeafQ x
compress (NodeQ q1 q2 q3 q4) = transformar (compress q1) (compress q2) (compress q3) (compress q4)

transformar :: QuadTree a -> QuadTree a -> QuadTree a -> QuadTree a -> QuadTree a
transformar (LeafQ x) (LeafQ y) (LeafQ z) (LeafQ w) = if (x == y) == (z == w) then LeafQ x else NodeQ (LeafQ x) (LeafQ y) (LeafQ z) (LeafQ w)
transformar q1 q2 q3 q4 = NodeQ q1 q2 q3 q4

qt = NodeQ (LeafQ 4) (LeafQ 4) (LeafQ 4) (LeafQ 4)

-- v. , que describe el
-- árbol resultante de transformar en nodo (manteniendo el dato de la hoja correspondiente) todas aquellas hojas que no se encuentren en el nivel de la altura del árbol.

uncompress :: QuadTree a -> QuadTree a
uncompress quadTree = descomprimir (heightQT quadTree) quadTree

descomprimir :: Int -> QuadTree a -> QuadTree a
descomprimir 0 (LeafQ x)           = (LeafQ x)
descomprimir 0 _                   = error "No se puede sacar"
descomprimir n (LeafQ x)           = NodeQ (descomprimir (n-1) (LeafQ x)) (descomprimir (n-1) (LeafQ x)) (descomprimir (n-1) (LeafQ x)) (descomprimir (n-1) (LeafQ x))
descomprimir n (NodeQ q1 q2 q3 q4) = NodeQ (descomprimir (n-1) q1) (descomprimir (n-1) q2) (descomprimir (n-1) q3) (descomprimir (n-1) q4)

