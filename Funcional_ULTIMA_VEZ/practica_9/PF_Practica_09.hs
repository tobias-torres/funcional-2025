data EA = Const Int | BOp BinOp EA EA deriving (Show)

data BinOp = Sum | Mul deriving (Show)

data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA deriving (Show)

-- f (Const n)         = ...
-- f (BOp binop e1 e2) = ... f e1 ... f e2

evalEA :: EA -> Int
evalEA (Const n)         = n
evalEA (BOp binop e1 e2) = checkOP binop (evalEA e1) (evalEA e2)

checkOP :: BinOp -> Int -> Int -> Int
checkOP Sum n1 n2 = n1 + n2
checkOP Mul n1 n2 = n1 * n2

ea2ExpA :: EA -> ExpA
ea2ExpA (Const n)         = Cte n
ea2ExpA (BOp binop e1 e2) = analizarOP binop (ea2ExpA e1) (ea2ExpA e2)

analizarOP :: BinOp -> ExpA -> ExpA -> ExpA
analizarOP Sum e1 e2 = Suma e1 e2
analizarOP Mul e1 e2 = Prod e1 e2

expA2ea :: ExpA -> EA
expA2ea (Cte n)          = Const n
expA2ea (Suma exp1 exp2) = BOp Sum (expA2ea exp1) (expA2ea exp2)
expA2ea (Prod exp1 exp2) = BOp Mul (expA2ea exp1) (expA2ea exp2)

-- i) ea2ExpA . expA2ea = id ? 
-- por ppio de extensionalidad
-- para todo exp. (ea2ExpA . expA2ea) exp = id exp ?
-- por def de (.)
-- para todo exp. ea2ExpA (expA2ea exp) = id exp ?
-- por def de id.
-- para todo exp. ea2ExpA (expA2ea exp) = exp ?
-- sea expa un elemento de tipo ExpA. por ppio de induccion sobre la estructura de expa, es equivalente demostrar que:

-- CASO BASE: expa = (Cte n)

-- ea2ExpA (expA2ea (Cte n)) = (Cte n) ?

-- CASO INDUCTIVO 1: expa = (Suma e1 e2)

-- HI1) ea2ExpA (expA2ea e1) = e1 ?
-- HI2) ea2ExpA (expA2ea e2) = e2 ?
-- TI) ea2ExpA (expA2ea (Suma e1 e2)) = (Suma e1 e2) ?

-- CASO INDUCTIVO 2: expa = (Prod e1 e2)

-- HI1) ea2ExpA (expA2ea e1) = e1 ?
-- HI2) ea2ExpA (expA2ea e2) = e2 ?
-- TI) ea2ExpA (expA2ea (Prod e1 e2)) = (Prod e1 e2) ?

-- CASO BASE:

-- LI)

-- ea2ExpA (expA2ea (Cte n))
--         -----------------
-- =                               def expA2ea
-- ea2ExpA (Const n)
-- -----------------
-- =                               def ea2ExpA
-- (Cte n)

-- LD)

-- (Cte n)

-- CASO INDUCTIVO 1:

-- LI)

-- ea2ExpA (expA2ea (Suma e1 e2))
--         ---------------------
-- =                               def expA2ea
-- ea2ExpA (BOp Sum (expA2ea exp1) (expA2ea exp2))
-- -----------------------------------------------
-- =                               def ea2ExpA
-- analizarOP Sum (ea2ExpA e1) (ea2ExpA e2)
-- ----------------------------------------
-- =                               de analizarOP
-- Suma (ea2ExpA e1) (ea2ExpA e2)
--      -----------  ------------
-- =                               HI1, HI2
-- (Suma e1 e2)

-- LD)

-- (Suma e1 e2)

-- CASO INDUCTIVO 2:

-- LI)

-- ea2ExpA (expA2ea (Prod e1 e2))
--         ---------------------
-- =                               def expA2ea
-- ea2ExpA (BOp Mul (expA2ea exp1) (expA2ea exp2))
-- -----------------------------------------------
-- =                               def ea2ExpA
-- analizarOP Mul (ea2ExpA e1) (ea2ExpA e2)
-- ----------------------------------------
-- =                               de analizarOP
-- Prod (ea2ExpA e1) (ea2ExpA e2)
--      -----------  ------------
-- =                               HI1, HI2
-- (Prod e1 e2)

-- LD)

-- (Prod e1 e2)

-- ii) expA2ea . ea2ExpA = id ?

-- por ppio de extensionalidad
-- para todo exparitm. (expA2ea . ea2ExpA) exparitm= id exparitm ?
-- por def de (.)
-- para todo exparitm. expA2ea (ea2ExpA exparitm) = id exparitm ?
-- por def de id.
-- para todo exparitm. expA2ea (ea2ExpA exparitm) = exparitm ?
-- sea ea de tipo EA, por ppio de induccion sobre la estructura de ea, es equivalente demostrar que:

-- CASO BASE: ea = Const n)

-- expA2ea (ea2ExpA (Const n)) = (Const n) ?

-- CASO INDUCTIVO : ea = BOp binop e1 e2)

-- subcaso: binop = Sum

-- HI1) expA2ea (ea2ExpA e1) = e1 !
-- HI2) expA2ea (ea2ExpA e2) = e2 !
-- TI) expA2ea (ea2ExpA (BOp Sum e1 e2)) = BOp Sum e1 e2 ?

-- subcaso: binop = Mul

-- HI1) expA2ea (ea2ExpA e1) = e1 !
-- HI2) expA2ea (ea2ExpA e2) = e2 !
-- TI) expA2ea (ea2ExpA (BOp Mul e1 e2)) = BOp Mul e1 e2 ?

-- CASO BASE:

-- LI)

-- expA2ea (ea2ExpA (Const n))
--         ------------------
-- =                               def ea2ExpA
-- expA2ea (Cte n)
-- ---------------
-- =                               def expA2ea
-- (Const n)


-- LD) 

-- (Const n)

-- CASO INDUCTIVO

-- subcaso 1 : binop = Sum

-- LI)

-- expA2ea (ea2ExpA (BOp Sum e1 e2))
--         ------------------------
-- =                               def ea2ExpA
-- expA2ea (analizarOP Sum (ea2ExpA e1) (ea2ExpA e2))
--         ------------------------------------------
-- =                               def analizarOP
-- expA2ea (Suma (ea2ExpA e1) (ea2ExpA e2))   
-- ----------------------------------------
-- =                               def expA2ea
-- BOp Sum (expA2ea exp1) (expA2ea exp2)
--         --------------  ------------
-- =                               HI1, HI2
-- BOp Sum e1 e2

-- LD)

-- (BOp Sum e1 e2)

-- subcaso 2 : binop = Mul

-- LI)

-- expA2ea (ea2ExpA (BOp Mul e1 e2))
--         ------------------------
-- =                               def ea2ExpA
-- expA2ea (analizarOP Mul (ea2ExpA e1) (ea2ExpA e2))
--         -----------------------------------------
-- =                               def analizarOP
-- expA2ea (Prod (ea2ExpA e1) (ea2ExpA e2))
-- ----------------------------------------
-- =                               def expA2ea
-- BOp Mul (ea2ExpA e1) (ea2ExpA e2)
--         -----------   ----------
-- =                               HI1, HI2
-- BOp Mul e1 e2

-- LD)

-- BOp Mul e1 e2 


-- iii) evalExpA . ea2ExpA = evalEA ?
-- por ppio de extensionalidad.
-- para todo evalAritm. (evalExpA . ea2ExpA) evalAritm = evalEA evalAritm ?
-- por def de (.)
-- para todo evalAritm. evalExpA (ea2ExpA evalAritm) = evalEA evalAritm ?
-- sea ea de tipo EA, por ppio de induccion sobre la estructura de ea, es equivalente demostrar que:

-- CASO BASE: ea = Const n)

-- evalExpA (ea2ExpA (Const n)) = evalEA (Const n) ?

-- CASO Inductivo: ea = BOp binop e1 e2)

-- subcaso 1 = binop Sum

-- HI1) evalExpA (ea2ExpA e1) = evalEA e1 !
-- HI2) evalExpA (ea2ExpA e2) = evalEA e2 !
-- TI) evalExpA (ea2ExpA (BOp Sum e1 e2)) = evalEA (BOp Sum e1 e2) ?

-- subcaso 2 = binop Mul

-- HI1) evalExpA (ea2ExpA e1) = evalEA e1 !
-- HI2) evalExpA (ea2ExpA e2) = evalEA e2 !
-- TI) evalExpA (ea2ExpA (BOp Mul e1 e2)) = evalEA (BOp Mul e1 e2) ?

-- Caso Base:

-- LI)

-- evalExpA (ea2ExpA (Const n))
--          -------------------
-- =                               def ea2ExpA
-- evalExpA (Cte n)
-- ---------------
-- =                               def evalExpA
-- n

-- LD)

-- evalEA (Const n)
-- ----------------
-- =                               def evalEA
-- n

-- Caso inductivo: 

-- subcaso 1 = binop Sum

-- LI)

-- evalExpA (ea2ExpA (BOp Sum e1 e2)) 
--           -----------------------
-- =                               def ea2ExpA
-- evalExpA (analizarOP Sum (ea2ExpA e1) (ea2ExpA e2))
--          -----------------------------------------
-- =                               def analizarOP
-- evalExpA (Suma (ea2ExpA e1) (ea2ExpA e2))
-- -----------------------------------------
-- =                               def evalExpA
-- evalExpA (ea2ExpA e1) + evalExpA (ea2ExpA e2)
-- --------------------    ---------------------
-- =                               HI1, HI2
-- evalEA e1 + evalEA e2


-- LD)

-- evalEA (BOp Sum e1 e2)
-- ---------------------
-- =                               def evalEA
-- checkOP Sum (evalEA e1) (evalEA e2)
-- -----------------------------------
-- =                               def checkOP
-- (evalEA e1) + (evalEA e2)


-- subcaso 2 : binop = Mul

-- LI)

-- evalExpA (ea2ExpA (BOp Mul e1 e2))
--           -----------------------
-- =                               def ea2ExpA
-- evalExpA (analizarOP Mul (ea2ExpA e1) (ea2ExpA e2))
--          ------------------------------------------
-- =                               def analizarOP
-- evalExpA (Prod (ea2ExpA e1) (ea2ExpA e2))
-- -----------------------------------------
-- =                               def evalExpA
-- evalExpA (ea2ExpA e1) * evalExpA (ea2ExpA e2)
-- ---------------------   ---------------------
-- =                               HI1, HI2
-- (evalEA e1) * (evalEA e2)

-- LD)

-- evalEA (BOp Mul e1 e2)
-- ----------------------
-- =                               def evalEA
-- checkOP Mul (evalEA e1) (evalEA e2)
-- -----------------------------------
-- =                               def checkOP
-- (evalEA e1) * (evalEA e2)

data Arbol a b = Hoja b | Nodo a (Arbol a b) (Arbol a b)

arbolAB = Nodo 10 (Hoja 2) (Nodo 22 (Hoja 2) (Hoja 33))

-- f Hoja x       = ...
-- f Nodo x t1 t2 = ... f t1 ... f t2

cantidadDeHojas :: Arbol a b -> Int
cantidadDeHojas (Hoja n)       = 1
cantidadDeHojas (Nodo x t1 t2) = cantidadDeHojas t1 + cantidadDeHojas t2

cantidadDeNodos :: Arbol a b -> Int
cantidadDeNodos (Hoja n)       = 0
cantidadDeNodos (Nodo x t1 t2) = 1 + cantidadDeNodos t1 + cantidadDeNodos t2 

cantidadDeConstructores :: Arbol a b -> Int
cantidadDeConstructores (Hoja n)       = 1
cantidadDeConstructores (Nodo x t1 t2) = 1 + cantidadDeConstructores t1 + cantidadDeConstructores t2

ea2Arbol :: EA -> Arbol BinOp Int
ea2Arbol (Const n)         = Hoja n
ea2Arbol (BOp binOp e1 e2) = Nodo binOp (ea2Arbol e1) (ea2Arbol e2) 


data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving (Show)

-- f EmptyT          = ...
-- f (NodeT x t1 t2) = ... f t1 ... f t2 ... 

treeA :: Tree Int
treeA = NodeT 10 (NodeT 10 EmptyT EmptyT) (NodeT 12 (NodeT 33 EmptyT EmptyT) EmptyT)

sumarT :: Tree Int -> Int
sumarT EmptyT          = 0
sumarT (NodeT x t1 t2) = x + sumarT t1 + sumarT t2 

sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT x t1 t2) = 1 + sizeT t1 + sizeT t2 

anyT :: (a -> Bool) -> Tree a -> Bool
anyT f EmptyT          = False
anyT f (NodeT x t1 t2) = f x || anyT f t1 || anyT f t2 


countT :: (a -> Bool) -> Tree a -> Int
countT f EmptyT          = 0
countT f (NodeT x t1 t2) = if f x then 1 + (countT f t1) + (countT f t2) else (countT f t1) + (countT f t2)


countLeaves :: Tree a -> Int
countLeaves EmptyT                  = 0
countLeaves (NodeT x EmptyT EmptyT) = 1
countLeaves (NodeT x t1 t2)         = countLeaves t1 + countLeaves t2 

heightT :: Tree a -> Int
heightT EmptyT          = 0
heightT (NodeT x t1 t2) = 1 + max (heightT t1) (heightT t2)

inOrder :: Tree a -> [a]
inOrder EmptyT          = []
inOrder (NodeT x t1 t2) =  inOrder t1 ++ [x] ++ inOrder t2

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT x t1 t2) = [x] : concatPerLevel (listPerLevel t1) (listPerLevel t2)

concatPerLevel :: [[a]] -> [[a]] -> [[a]]
concatPerLevel [] yss            = yss
concatPerLevel xss []            = xss
concatPerLevel (xs:xss) (ys:yss) = (xs ++ ys) : concatPerLevel xss yss

mirrorT :: Tree a -> Tree a
mirrorT EmptyT          = EmptyT
mirrorT (NodeT x t1 t2) = NodeT x (mirrorT t2) (mirrorT t1)

levelN :: Int -> Tree a -> [a]
levelN n EmptyT          = []
levelN 0 (NodeT x t1 t2) = [x]
levelN n (NodeT x t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT x t1 t2) = if length (ramaMasLarga t1) > length (ramaMasLarga t2) 
                                    then x : ramaMasLarga t1 
                                    else x : ramaMasLarga t2

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT                  = []
todosLosCaminos (NodeT x EmptyT EmptyT) = [[x]]
todosLosCaminos (NodeT x t1 t2)         = agregarRaiz x (todosLosCaminos t1 ++ todosLosCaminos t2)

agregarRaiz :: a -> [[a]] -> [[a]]
agregarRaiz x []       = []
agregarRaiz x (xs:xss) = (x:xs) : agregarRaiz x xss

-- i. 

-- Prop) 

-- heightT = length . ramaMasLarga ?

-- Dem)
-- por ppio de extensionalidad
-- para todo tree. heightT tree = (length . ramaMasLarga) tree ?
-- por def de (.)
-- para todo tree. heightT tree = length (ramaMasLarga tree) ?
-- sea arbol de tipo Tree a, por ppio de induccion sobre la estructura de arbol. Es equivalente demostrar que:

-- CASO BASE: arbol = EmptyT

-- heightT EmptyT = length (ramaMasLarga EmptyT) ?

-- CASO INDUCTIVO: arbol = NodeT x t1 t2)

-- HI1) heightT t1 = length (ramaMasLarga t1) !
-- HI2) heightT t2 = length (ramaMasLarga t2) !
-- TI) heightT (NodeT x t1 t2) = length (ramaMasLarga (NodeT x t1 t2)) ?

-- CASO BASE:

-- LI)

-- heightT EmptyT
-- --------------
-- =               def heightT
-- 0

-- LD)

-- length (ramaMasLarga EmptyT)
--         -------------------
-- =                           def ramaMasLarga EmptyT
-- length []
-- ---------
-- =               def length
-- 0

-- CASO INDUCTIVO:

-- LI)

-- heightT (NodeT x t1 t2)
-- -----------------------
-- =               def heightT
-- 1 + max (heightT t1) (heightT t2)
--     -----------------------------
-- =               def max
-- 1 + if (heightT t1 > heightT t2) then heightT t1 else heightT t2
-- ----------------------------------------------------------------
-- =               def de prop f (if b then y else z) = if b then f y else f z
-- if (heightT t1 > heightT t2) then 1 + heightT t1 else 1 + heightT t2
--                                       ----------          ----------
-- =               HI1, HI2
-- if (heightT t1 > heightT t2) then 1 + length (ramaMasLarga t1) else 1 + length (ramaMasLarga t2)

-- LD)

-- length (ramaMasLarga (NodeT x t1 t2))
--         ----------------------------
-- =               def ramaMasLarga
-- length ((if heightT t1 >  heightT t2) then x : ramaMasLarga t1 else x : ramaMasLarga t2)
-- ----------------------------------------------------------------------------------------
-- =               def de prop f (if b then y else z) = if b then f y else f z
-- if heightT t1 > heightT t2 then length (x : ramaMasLarga t1) else length (x : ramaMasLarga t2)
--                                 ---------------------------       ----------------------------
-- =               def de length
-- if heightT t1 > heightT t2 then 1 + length (ramaMasLarga t1) else 1 + 1 + length (ramaMasLarga t2)

-- f (if b then y else z) = if b then f y else f z

-- ii)
-- Prop) reverse . inOrder = inOrder . mirrorT ?
-- por ppio de extensionalidad
-- para todo tree. (reverse . inOrder) tree = (inOrder . mirrorT) tree ?
-- por def de (.) dos veces.
-- para todo tree. reverse (inOrder tree) = inOrder (mirrorT tree) ?
-- sea treeA de tipo Tree a, por ppio de induccion sobre la estructura de treeA. Es equivalente demostrar que:

-- CASO BASE: treeA = EmptyT

-- reverse (inOrder EmptyT) = inOrder (mirrorT EmptyT) ?

-- CASO INDUCTIVO: treeA = (NodeT x t1 t2)

-- HI1) reverse (inOrder t1) = inOrder (mirrorT t1) !
-- HI2) reverse (inOrder t2) = inOrder (mirrorT t2) !
-- TI) reverse (inOrder (NodeT x t1 t2)) = inOrder (mirrorT (NodeT x t1 t2)) ?


-- CASO BASE:

-- LI)

-- reverse (inOrder EmptyT)
--         ----------------
-- =                       def inOrder
-- reverse []
-- ----------
-- =                       def reverse
-- [] 

-- LD)

-- inOrder (mirrorT EmptyT)
--         ----------------
-- =                       def mirrorT
-- inOrder EmptyT
-- --------------
-- =                       def inOrder
-- []

-- CASO INDUCTIVO

-- LI)

-- reverse (inOrder (NodeT x t1 t2)) 
--         ------------------------
-- =                       def inOrder.
-- reverse (inOrder t1 ++ [x] ++ inOrder t2)
-- -----------------------------------------
-- =                       def lema reverse-distributivo.
-- reverse (inOrder t2) ++ [x] ++ reverse (inOrder t1)


-- LD)

-- inOrder (mirrorT (NodeT x t1 t2))
--         ------------------------
-- =                       def mirrorT
-- inOrder (NodeT x (mirrorT t2) (mirrorT t1))
-- -------------------------------------------
-- =                       def inOrder
-- inOrder (mirrorT t2) ++ [x] ++ inOrder (mirrorT t1)
-- --------------------           --------------------
-- =                       HI1, HI2
-- reverse (inOrder t2) ++ [x] ++ reverse (inOrder t1)


-- lema reverse-distributivo.

-- por ppio de extensionalidad
-- para todo xs. para todo ys. reverse (xs ++ [x] ++ ys) = reverse (ys) ++ [x] ++ reverse xs
-- sean js y ps dos listas finitas, por ppio de induccion sobre la estructura de js, es equivalente demostrar que:

-- CASO BASE: js = []

-- reverse ([] ++ [x] ++ ps) = reverse ps ++ [x] ++ reverse [] ?

-- CASO INDUCTIVO: js = (x:xs')

-- HI) reverse (xs' ++ [x] ++ ps) = reverse ps ++ [x] ++ reverse xs' !
-- TI) reverse ((x:xs') ++ [x] ++ ps) = reverse ps ++ [x] ++ reverse (x:xs') ?

-- CASO BASE:

-- LI)

-- reverse ([] ++ [x] ++ ps) 
--                 --------
-- =                       def ++
-- reverse ([] ++ x : ([] ++ ps))
--                    ----------
-- =                       def ++
-- reverse ([] ++ x : ps)
--          ------------
-- =                       def ++
-- reverse (x:ps)
-- --------------
-- =                       def reverse
-- reverse ps ++ [x]

-- LD)

-- reverse ps ++ [x] ++ reverse []
--                      ----------
-- =                       def reverse
-- reverse ps ++ [x] ++ []
--               ----------
-- =                       def ++
-- reverse ps ++ [x]


-- CASO INDUCTIVO:

-- LI)

-- reverse ((x:xs') ++ [x] ++ ps)
--                     ---------
-- =                       def ++
-- reverse ((x:xs') ++ x : ([] ++ ps))
--                         ----------
-- =                       def ++
-- reverse ((x:xs') ++ x : ps)
--         -------------------
-- =                       def ++
-- reverse (x : (xs' (++) (x : ps)))
-- ---------------------------------
-- =                       def reverse
-- reverse (xs' ++ (x :ps)) ++ [x]


-- LD)

-- reverse ps ++ [x] ++ reverse (x:xs')
--                      ---------------
-- =                       def reverse
-- reverse ps ++ [x] ++ reverse xs' ++ [x]
-- -------------------------------
-- =                       HI
-- reverse (xs' ++ [x] ++ ps) ++ [x]
--                 ---------
-- =                       def ++
-- reverse (xs' ++ x : ([] ++ ps)) ++ [x]
--                     ---------
-- =                       def ++
-- reverse (xs' ++ x : ps) ++ [x]

data AppList a = Single a | Append (AppList a) (AppList a) deriving (Show)

-- f (Single x)         = ...
-- f (Append apl1 apl2) = ... f apl1 ... f apl2

single = Append (Single 3) (Single 8)

single2 = Append (Single 2) (Append (Single 3) (Single 4))

lenAL :: AppList a -> Int
lenAL (Single x)         = 1
lenAL (Append apl1 apl2) = lenAL apl1 + lenAL apl2

consAL :: a -> AppList a -> AppList a
consAL e (Single x)         = Append (Single e) (Single x)
consAL e (Append apl1 apl2) = Append (consAL e apl1) apl2

consAL' :: a -> AppList a -> AppList a
consAL' x = Append (Single x)

headAL :: AppList a -> a
headAL (Single x)         = x
headAL (Append apl1 apl2) = headAL apl1

tailAL :: AppList a -> AppList a
tailAL (Single x)               = error "Tiene un solo elemento"
tailAL (Append (Single x) apl2) = apl2
tailAL (Append apl1 apl2)       = Append (tailAL apl1) apl2

snocAL :: AppList a -> a -> AppList a
snocAL (Single x) e         = Append (Single x) (Single e)
snocAL (Append apl1 apl2) e = Append apl1 (snocAL apl2 e)

lastAL :: AppList a -> a
lastAL (Single x)         = x
lastAL (Append apl1 apl2) = lastAL apl2

-- lastB :: [a] -> a
-- lastB []     = error "no existe elemento para sacar"
-- lastB [x]    = x
-- lastB (x:xs) = lastB xs

initAL :: AppList a -> AppList a
initAL (Single x)               = error "Tiene un solo elemento"
initAL (Append apl1 (Single x)) = apl1
initAL (Append apl1 apl2)       = Append apl1 (initAL apl2)

reverseAL :: AppList a -> AppList a
reverseAL (Single x) = (Single x)
reverseAL (Append apl1 apl2) = Append (reverseAL apl1) (reverseAL apl2)

elemAL :: Eq a => a -> AppList a -> Bool
elemAL e (Single x) = e == x
elemAL e (Append apl1 apl2) = (elemAL e apl1) || (elemAL e apl2)

appendAL :: AppList a -> AppList a -> AppList a     
appendAL = Append 

appListToList :: AppList a -> [a]
appListToList (Single x)         = [x]
appListToList (Append apl1 apl2) = appListToList apl1 ++ appListToList apl2


-- ### DEMOSTRACIONES ###

-- i. para todo xs :: AppList a. para todo ys :: AppList a. lenAL (appendAL xs ys) = lenAL xs + lenAL ys ?
-- sean zs y ws dos elementos de tipo AppList a, por ppio de induccion sobre la estructura de zs, es equivalente desmotrar que

-- CASO BASE: zs = Single x)

-- lenAL (appendAL (Single x) ws) = lenAL (Single x) + lenAL ws ?

-- CASO INDUCTIVO: zs = Append apl1 apl2)

-- HI1) lenAL (appendAL apl1 ws) = lenAL apl1 + lenAL ws ?
-- HI2) lenAL (appendAL apl2 ws) = lenAL apl2 + lenAL ws ?
-- TI)  lenAL (appendAL (Append apl1 apl2) ws) = lenAL (Append apl1 apl2) + lenAL ws ?


-- CASO BASE:

-- LI)

-- lenAL (appendAL (Single x) ws)
--         ---------------------
-- =               def appendAL
-- lenAL (Append ws (Single x))
-- ----------------------------
-- =               def lenAL
-- lenAL ws + lenAL (Single x)

-- LD)

-- lenAL (Single x) + lenAL ws 

-- CASO INDUCTIVO:

-- LI)

-- lenAL (appendAL (Append apl1 apl2) ws)
--         -----------------------------
-- =               def appendAL
-- lenAL (Append ws (Append apl1 apl2))
-- ------------------------------------
-- =               def lenAL
-- lenAL ws + lenAL (Append apl1 apl2)
--             -----------------------
-- =               def lenAL
-- lenAL ws + lenAL apl1 + lenAL apl2


-- LD)

-- lenAL (Append apl1 apl2) + lenAL ws
-- -----------------------
-- =               def lenAL
-- lenAL apl1 + lenAL apl2 + lenAL ws

-- ii) Prop: reverseAL . reverseAL = id ?
-- Dem)
-- por ppio de extensionalidad
-- para todo apl. (reverseAL . reverseAL) apl = id apl ?
-- por def de (.)
-- para todo apl. reverseAL (reverseAL apl) = id apl ?
-- por def de id
-- para todo apl. reverseAL (reverseAL apl) = apl ?
-- sea appL de tipo AppList a, por ppio de induccion sobre la estructura de appL, es equivalente demostrar que:

-- CASO BASE: appL = Single x)

-- reverseAL (reverseAL (Single x)) = (Single x) ?

-- CASO INDUCTIVO: appL = (Append apl1 apl2)

-- HI1) reverseAL (reverseAL apl1) = apl1 !
-- HI2) reverseAL (reverseAL apl2) = apl2 !
-- TI) reverseAL (reverseAL (Append apl1 apl2)) = (Append apl1 apl2) ?

-- CASO BASE: 

-- LI)

-- reverseAL (reverseAL (Single x)) 
--             -------------------
-- =               def reverseAL
-- reverseAL (Single x)
-- --------------------
-- =               def reverseAL
-- (Single x)

-- LD)

-- (Single x)

-- CASO INDUCTIVO:

-- LI)

-- reverseAL (reverseAL (Append apl1 apl2))
--             ---------------------------
-- =               def reverseAL
-- reverseAL (Append (reverseAL apl2) (reverseAL apl1))
-- ---------------------------------------------------
-- =               def reverseAL
-- Append (reverseAL (reverseAL apl1)) (reverseAL (reverseAL apl2))
--         --------------------------  ---------------------------
-- =               HI1, HI2
-- Append apl1 apl2

-- LD)

-- (Append apl1 apl2) 


-- (reverseAL .) . flip consAL . reverseAL = snocAL
-- por ppio de extensionalidad
-- para todo apl. para todo x. ((reverseAL .) . flip consAL . reverseAL) apl x = snocAL apl x ?
-- por def de (.)
-- para todo apl. para todo x. ((reverseAL .). flip consAL) (reverseAL apl) x = snocAL apl x ?
-- por def de (.)
-- para todo apl. para todo x. (reverseAL . flip consAL (reverseAL apl)) x = snocAL apl x ?
-- por def de (.)
-- para todo apl. para todo x. reverseAL (flip consAL (reverseAL apl ) x) = snocAL apl x ?
-- por def de flip
-- para todo apl. para todo x. reverseAL (consAL x (reverseAL applist))
-- por def de consAL
-- para todo apl. para todo x. reverseAL (Append (Single x) (reverseAL applist)) 
-- por def de reverseAL
-- para todo apl. para todo x. Append (reverseAL (reverseAL applist)) (reverseAL (Single x))
-- def de reverseAL
-- para todo apl. para todo x. Append (reverseAL (reverseAL applist)) (Single x)
-- por dem anterior
-- para todo apl. para todo x. Append applist (Single x)



data QuadTree a = LeafQ a | NodeQ (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a) deriving (Show)

data Color = RGB Int Int Int deriving (Show, Eq)

type Image = QuadTree Color

-- f (LeafQ x)               = ...
-- f (NodeQ qt1 qt2 qt3 qt4) = ... (f qt1) ... (f qt2) ... (f qt3) ... (f qt4)

quadTree = NodeQ (LeafQ (RGB 1 2 3)) (LeafQ (RGB 1 2 3)) (LeafQ (RGB 1 2 3)) (LeafQ (RGB 1 2 3))

heightQT :: QuadTree a -> Int
heightQT (LeafQ x)               = 0
heightQT (NodeQ qt1 qt2 qt3 qt4) = 1 + (heightQT qt1) `max` (heightQT qt2) `max` (heightQT qt3) `max` (heightQT qt4)

countLeavesQT :: QuadTree a -> Int
countLeavesQT (LeafQ x)               = 1
countLeavesQT (NodeQ qt1 qt2 qt3 qt4) = (heightQT qt1) + (heightQT qt2) + (heightQT qt3) + (heightQT qt4)

sizeQT :: QuadTree a -> Int
sizeQT (LeafQ x)               = 1
sizeQT (NodeQ qt1 qt2 qt3 qt4) = 1 + (sizeQT qt1) + (sizeQT qt2) + (sizeQT qt3) + (sizeQT qt4)

compress :: Eq a => QuadTree a -> QuadTree a
compress (LeafQ x)               = (LeafQ x)
compress (NodeQ qt1 qt2 qt3 qt4) = compressTodo (compress qt1) (compress qt2) (compress qt3) (compress qt4) 

compressTodo :: Eq a => QuadTree a -> QuadTree a -> QuadTree a -> QuadTree a -> QuadTree a
compressTodo (LeafQ n1) (LeafQ n2) (LeafQ n3) (LeafQ n4) = if (n1 == n2) == (n3 == n4)
                                                            then LeafQ n1
                                                            else NodeQ (LeafQ n1) (LeafQ n2) (LeafQ n3) (LeafQ n4)
compressTodo lq1 lq2 lq3 lq4                             = NodeQ lq1 lq2 lq3 lq4

uncompress :: Eq a => QuadTree a -> QuadTree a
uncompress qtree = descomprimirT (heightQT qtree) qtree

descomprimirT :: Int -> QuadTree a -> QuadTree a
descomprimirT 0 (LeafQ x)             = (LeafQ x)
descomprimirT 0 _                     = error "no podes sacarlo"
descomprimirT n (LeafQ x)             = NodeQ (descomprimirT (n-1) (LeafQ x)) (descomprimirT (n-1) (LeafQ x)) (descomprimirT (n-1) (LeafQ x)) (descomprimirT (n-1) (LeafQ x)) 
descomprimirT n (NodeQ q1 q2 q3 q4) = NodeQ (descomprimirT (n-1) q1) (descomprimirT (n-1) q2) (descomprimirT (n-1) q3) (descomprimirT (n-1) q4)  

logBase :: Int -> Int -> Int
logBase = (^)