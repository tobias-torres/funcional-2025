import Prelude hiding (map, filter, recr, foldr1, zipWith, scanr, length,any, all, countBy, partition, zipWith, scanr, takeWhile, take, drop, elemAt)

data N = Z | S N deriving (Show)

dos = S (S Z)

tres = S (S (S Z))

-- , que describe el número representado por el elemento dado.
evalN :: N -> Int
evalN Z     = 0
evalN (S n) = 1 + evalN n

-- , que describe la representación unaria de la suma  de  los  números  representados  por  los  argumentos.  La 
-- resolución  debe  ser  exclusivamente  simbólica, o sea, SIN calcular cuáles son esos números. 
addN :: N -> N -> N
addN Z n     = n
addN (S n) m = S (addN n m)

-- que describe la representación unaria del producto  de  los  números  representados  por  los  argumentos.  La 
-- resolución debe ser exclusivamente simbólica. 
prodN :: N -> N -> N
prodN Z n     = Z 
prodN (S n) m = addN m (prodN n m)

int2N :: Int -> N
int2N 0 = Z
int2N n = S (int2N (n-1))

type NU = [()]

data Unit = Unit 

cuatro = [(),(),(),()]

-- , que describe el número representado por el elemento dado. 
evalNU :: NU -> Int
evalNU []     = 0
evalNU (n:ns) = 1 + evalNU ns

-- , que describe la representación unaria del 
-- resultado de sumarle uno al número representado por el argumento. 
-- La resolución debe ser exclusivamente simbólica. 
succNU :: NU -> NU
succNU ns = () : ns

-- , que describe la representación unaria de la suma de los números representados por los argumentos. La 
-- resolución debe ser exclusivamente simbólica. 
addNU :: NU -> NU -> NU
addNU [] nu      = nu
addNU (n:nu) nu' = n : (addNU nu nu')

-- , que describe la representación unaria dada por el tipo N correspondiente al número representado por el argumento. 
nu2n :: NU -> N
nu2n []     = Z
nu2n (n:ns) = S (nu2n ns)

-- , que describe la representación unaria dada por el tipo NU correspondiente al número representado por el argumento. 
n2nu :: N -> NU
n2nu Z     = []
n2nu (S n) = () : n2nu n


type NBin = [DigBin]

data DigBin = O | I deriving (Show)

-- a.dado un símbolo que representa un dígito binario lo transforma en su significado como número
dbAsInt :: DigBin -> Int
dbAsInt O = 0
dbAsInt I = 1

-- b. que dado un símbolo que representa un dígito binario lo transforma en su significado como booleano.
dbAsBool :: DigBin -> Bool
dbAsBool I = True
dbAsBool O = False

-- c. que dado un booleano lo transforma en el símbolo que representa a ese booleano.
dbOfBool :: Bool -> DigBin
dbOfBool True = I
dbOfBool False = O

-- d. que dado un dígito binario lo transforma en el otro.
negDB :: DigBin -> DigBin
negDB I = O
negDB O = I

nbin = [I,I,I]

-- , que describe el número representado por el elemento dado. 
evalNB :: NBin -> Int
evalNB []       = 0
evalNB (n:nbin) = (dbAsInt n) + 2 * (evalNB nbin)

-- que describe la representación binaria del número representado por el argumento, pero sin “ceros a 
-- la izquierda” (dígitos redundantes).  
-- OBSERVACIÓN:  por  la  forma  de  la  representación,  los  “ceros  a izquierda” aparecen a la derecha de la lista. Entonces la propiedad 
-- indica que una lista de dígitos normalizada no puede terminar con el dígito 0. 
normalizarNB :: NBin -> NBin
normalizarNB []       = []
normalizarNB (n:nbin) = normalizar n (normalizarNB nbin)

normalizar :: DigBin -> NBin -> NBin
normalizar O []   = []
normalizar d nbin = d : nbin

-- , que describe la representación binaria normalizada  del resultado de sumarle uno al número representado por el argumento. La resolución debe 
-- ser exclusivamente simbólica, y no  debe  utilizar  normalizarNB.  Se  puede  suponer  como precondición que el argumento está normalizado.
succNB :: NBin -> NBin
succNB []       = [I]
succNB (O:nbin) = I : nbin
succNB (I:nbin) = O : succNB nbin

-- ,  que  describe  la representación  binaria  normalizada  de  la  suma  de  los  números 
-- representados  por  los  argumentos.  La  resolución  debe  ser exclusivamente simbólica (o sea, no usar ninguna forma de eval), y 
-- no  debe  utilizar  normalizarNB.  Se  puede  suponer  como precondición que los argumentos están normalizados. 
-- AYUDA: considerar dos operaciones auxiliares
-- addNBConCarry :: NBin -> NBin -> DigBin -> NBin, y addDBConCarry :: DigBin -> DigBin -> DigBin -> (DigBin, DigBin).
addNB :: NBin -> NBin -> NBin
addNB [] nbin             = nbin
addNB nbin []             = nbin
addNB (n:nbin) (n':nbin') = agregar n n' (addNB nbin nbin')

agregar :: DigBin -> DigBin -> NBin -> NBin
agregar O O nbin = I : nbin
agregar I I nbin = O : succNB nbin
agregar _ _ nbin = I : nbin

-- que describe la representación unaria dada por  el  tipo  N  correspondiente  al  número  representado  por  el argumento. 
nb2n :: NBin -> N
nb2n []      = Z
nb2n (n:bin) = addN (evalD n) (prodN (S (S Z)) (nb2n bin) )

evalD O = Z
evalD I = (S Z)

-- ,  que  describe  la  representación  binaria normalizada  dada  por  el  tipo  NBin  correspondiente  al  número representado por el argumento
n2nb :: N -> NBin
n2nb Z     = []
n2nb (S n) = succNB (n2nb n)

type NDec = [DigDec] 

data DigDec = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 deriving (Show)

--      U   D
diez = [D0, D1]

veinte = [D0,D2]

--             U   D
veinticinco = [D5, D2]

--                 U   D   C
trescuatrocinco = [D5, D4, D3]

ddAsInt :: DigDec -> Int
ddAsInt D0 = 0
ddAsInt D1 = 1
ddAsInt D2 = 2
ddAsInt D3 = 3
ddAsInt D4 = 4
ddAsInt D5 = 5
ddAsInt D6 = 6
ddAsInt D7 = 7
ddAsInt D8 = 8
ddAsInt D9 = 9

ddOfInt :: Int -> DigDec
ddOfInt 0 = D0
ddOfInt 1 = D1
ddOfInt 2 = D2
ddOfInt 3 = D3
ddOfInt 4 = D4
ddOfInt 5 = D5
ddOfInt 6 = D6
ddOfInt 7 = D7
ddOfInt 8 = D8
ddOfInt 9 = D9

-- c. , que dado un dígito decimal lo transforma en el siguiente según el orden circular dado en la definición.
nextDD :: DigDec -> DigDec
nextDD D0 = D1
nextDD D1 = D2
nextDD D2 = D3
nextDD D3 = D4
nextDD D4 = D5
nextDD D5 = D6
nextDD D6 = D7
nextDD D7 = D8
nextDD D8 = D9
nextDD D9 = D0

-- d. , que dado un dígito decimal lo transforma en el anterior según el orden circular dado en la definición.
prevDD :: DigDec -> DigDec
prevDD D0 = D9
prevDD D1 = D0
prevDD D2 = D1
prevDD D3 = D2
prevDD D4 = D3
prevDD D5 = D4
prevDD D6 = D5
prevDD D7 = D6
prevDD D8 = D7
prevDD D9 = D8

evalND :: NDec -> Int
evalND []       = 0
evalND (n:ndec) = ddAsInt n + 10 * evalND ndec

normalizarND :: NDec -> NDec
normalizarND []       = []
normalizarND (n:ndec) = normalizarND' n (normalizarND ndec)

normalizarND' :: DigDec -> NDec -> NDec
normalizarND' D0 []  = []
normalizarND' d ndec = d : ndec

succNDec :: NDec -> NDec
succNDec []        = [D1]
succNDec (D9:ndec) = D0 : succNDec ndec
succNDec (n:ndec)  = nextDD n : ndec

-- que  describe la representación  decimal  normalizada  de  la  suma  de  los  números 
-- representados por  los  argumentos.  La  resolución  debe  ser exclusivamente  simbólica,  y  no  debe  utilizar
-- normalizarND. Se puede suponer como precondición que los argumentos está normalizados
addNDec :: NDec -> NDec -> NDec
addNDec ndec []            = ndec
addNDec [] ndec            = ndec
addNDec (n:ndec) (m:ndec') = sumarDigitos n m (addNDec ndec ndec')

sumarDigitos :: DigDec -> DigDec -> NDec -> NDec
sumarDigitos D0 y ndec = y : ndec
sumarDigitos x y ndec  = succNDec (sumarDigitos (prevDD x) y ndec)

nd2nb :: NDec -> NBin
nd2nb ndec = int2db (evalND ndec)

int2db :: Int -> NBin
int2db 0 = []
int2db n = if mod n 2 == 0 
            then O : int2db (div n 2)
            else I : int2db (div n 2)

nb2nd :: NBin -> NDec
nb2nd nbin = int2nd (evalNB nbin)

int2nd :: Int -> NDec
int2nd 0 = []
int2nd n = ddOfInt (mod n 10) : int2nd (div n 10)

-- Ejercicios del TP 9

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving (Show)

arbol = NodeT 10 (NodeT 20 EmptyT EmptyT)
                 (NodeT 30 (NodeT 44 EmptyT EmptyT) EmptyT)

heightT :: Tree a -> Int
heightT EmptyT          = 0
heightT (NodeT x t1 t2) = 1 + max (heightT t1) (heightT t2)

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT         = []
listPerLevel (NodeT x t t') = [x] : concatPerLevel (listPerLevel t) (listPerLevel t')

concatPerLevel :: [[a]] -> [[a]] -> [[a]]
concatPerLevel [] yss = yss
concatPerLevel xss [] = xss
concatPerLevel (xs:xss) (ys:yss) = (xs ++ ys) : concatPerLevel xss yss

-- x. que describe la lista con los elementos del nivel dado en el árbol dado.
levelN :: Int -> Tree a -> [a]
levelN n EmptyT          = []
levelN 0 (NodeT x _ _)   = [x]
levelN n (NodeT x t1 t2) = (levelN (n-1) t1) ++ (levelN (n-1) t2)

-- xi. que describe la lista con los elementos de la rama más larga del árbol.
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT         = []
ramaMasLarga (NodeT x t t') = if (heightT t) > (heightT t') 
                                then x : ramaMasLarga t
                                else x : ramaMasLarga t'

-- xii. que describe la lista con todos los caminos existentes en el árbol dado.
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT                  = []
todosLosCaminos (NodeT x EmptyT EmptyT) = [[x]]
todosLosCaminos (NodeT x t t')          = agregarRaiz x (todosLosCaminos t ++ todosLosCaminos t')

agregarRaiz :: a -> [[a]] -> [[a]]
agregarRaiz e []       = []
agregarRaiz e (xs:xss) = (e:xs) : (agregarRaiz e xss)

data AppList a = Single a | Append (AppList a) (AppList a) deriving (Show)

foldAL :: (a -> b) -> (b -> b -> b) -> AppList a -> b
foldAL fs fa (Single x)      = fs x
foldAL fs fa (Append ap ap') = fa (foldAL fs fa ap) (foldAL fs fa ap')

recAL :: (a -> b) -> (AppList a -> AppList a -> b -> b -> b) -> AppList a -> b
recAL fs fa (Single x)      = fs x
recAL fs fa (Append ap ap') = fa ap ap' (recAL fs fa ap) (recAL fs fa ap')

appendProof = Append (Single 3) (Append (Single 33) (Single 55))

-- , que describe la cantidad de elementos de la lista.
lenAL :: AppList a -> Int
lenAL (Single x)      = 1
lenAL (Append ap ap') = (lenAL ap) + (lenAL ap')

lenAL' :: AppList a -> Int
lenAL' = foldAL (const 1) (+)

-- , que describe la lista resultante de agregar el elemento dado al principio de la lista dada.
consAL :: a -> AppList a -> AppList a
consAL e (Single x)      = Append (Single e) (Single x)
consAL e (Append ap ap') = Append (consAL e ap) ap'

consAL' :: a -> AppList a -> AppList a
consAL' e = recAL (\x -> Append (Single e) (Single x)) (\ap ap' appList appList' -> Append appList ap')

-- , que describe el primer elemento de la lista dada.
headAL :: AppList a -> a
headAL (Single x)      = x
headAL (Append ap ap') = (headAL ap)

headAL' :: AppList a -> a
headAL' = foldAL id (\ap ap' -> ap)

-- -- , que describe la lista resultante de quitar el primer elemento de la lista dada.
tailAL :: AppList a -> AppList a
tailAL (Single x)             = error "no se puede sacar"
tailAL (Append (Single x) ap) = ap
tailAL (Append ap ap')        = Append (tailAL ap) ap'

tailAL' :: AppList a -> AppList a
tailAL' = recAL (\x -> error "no se puede sacar elem de una lista vacia") 
                 (\ap ap' appList appList' -> case ap of
                                Single _ -> ap'
                                Append _ _ -> Append appList ap')

-- -- , que describe la lista resultante de agregar el elemento dado al final de la lista dada.
snocAL :: AppList a -> a -> AppList a
snocAL (Single x) e      = Append (Single x) (Single e)
snocAL (Append ap ap') e = Append ap (snocAL ap' e)

snocAL' :: AppList a -> a -> AppList a
snocAL' = recAL (\x e -> Append (Single x) (Single e))
                 (\ap ap' appList appList' e -> Append ap (appList' e))

-- -- , que describe el último elemento de la lista dada.
lastAL :: AppList a -> a
lastAL (Single x)             = x
lastAL (Append ap ap')        = lastAL ap'

lastAL' :: AppList a -> a
lastAL' = foldAL id (const id)

-- -- , que describe la lista dada sin su último elemento.
initAL :: AppList a -> AppList a
initAL (Single x)             = error "Lista vacia"
initAL (Append ap (Single x)) = ap 
initAL (Append ap ap')        = Append ap (initAL ap')

initAL' :: AppList a -> AppList a
initAL' = recAL (\x -> error "no se puede sacar elem de una lista vacia")
               (\ap ap' appList appList' -> case ap' of
                                            Single _ -> ap
                                            Append _ _ -> Append ap appList')

-- , que describe la lista dada con sus elementos en orden inverso.
reverseAL :: AppList a -> AppList a
reverseAL (Single x)      = (Single x)
reverseAL (Append ap ap') = Append (reverseAL ap') (reverseAL ap)

reverseAL' :: AppList a -> AppList a
reverseAL' = foldAL Single (\ap ap' -> Append ap' ap)

-- -- , que indica si el elemento dado se encuentra en la lista dada.
elemAL :: Eq a => a -> AppList a -> Bool
elemAL e (Single x)      = e == x
elemAL e (Append ap ap') = elemAL e ap || elemAL e ap'

elemAL' :: Eq a => a -> AppList a -> Bool
elemAL' = flip (foldAL (\x e -> e == x) 
                       (\ap ap' e -> ap e || ap' e))

-- -- , quedescribe el resultado de agregar los elementos de la primera listaadelante de los elementos de la segunda. NOTA: buscar la manera más eficiente de hacerlo.
appendAL :: AppList a -> AppList a -> AppList a
appendAL = Append

-- -- , que describe la representación lineal de la lista dada.
appListToList :: AppList a -> [a]
appListToList (Single x)      = [x]
appListToList (Append ap ap') = appListToList ap ++ appListToList ap'

appListToList' :: AppList a -> [a]
appListToList' = foldAL (\x -> [x])
                        (++)

-- Practica 11

data Pizza = Prepizza | Capa Ingrediente Pizza deriving (Show)

data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamon | Queso | Salsa deriving (Show)

pz1 = Capa Salsa (Capa Anchoas Prepizza) 
pz2 = Capa Cebolla (Capa Queso (Capa Jamon Prepizza))
pz3 = Capa Cebolla (Capa (Aceitunas 10) (Capa Jamon Prepizza))

foldP :: (Ingrediente -> b -> b) -> b -> Pizza -> b
foldP fc fp (Capa i p) = fc i (foldP fc fp p)
foldP fc fp Prepizza   = fp 

cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int 
cantidadCapasQueCumplen p = foldP (\i n -> if p i then 1 + n else n) 0

conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza 
conCapasTransformadas f = foldP (\i p -> Capa (f i) p) Prepizza

soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue p = foldP (\i pzz -> if (p i) then Capa i pzz else pzz) Prepizza

-----------------------------------------------------------------------------------

sinLactosa :: Pizza -> Pizza
sinLactosa = soloLasCapasQue (not . esQueso)

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _     = False

aptaIntolerantesLactosa :: Pizza -> Bool 
-- aptaIntolerantesLactosa pz = (cantidadCapasQueCumplen esQueso pz) == 0
-- aptaIntolerantesLactosa pz = (==) (cantidadCapasQueCumplen esQueso pz) 0
-- aptaIntolerantesLactosa pz = (==0) (cantidadCapasQueCumplen esQueso pz)
aptaIntolerantesLactosa = (==0) . (cantidadCapasQueCumplen esQueso)

cantidadDeQueso :: Pizza -> Int 
cantidadDeQueso = cantidadCapasQueCumplen esQueso

conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas = conCapasTransformadas duplicarAceitunas

duplicarAceitunas :: Ingrediente -> Ingrediente
duplicarAceitunas (Aceitunas n) = Aceitunas (2*n)
duplicarAceitunas i             = i

-----------------------------------------------------------------------------------

sinLactosa' :: Pizza -> Pizza
sinLactosa' = foldP (\i p -> if (not (esQueso i)) then Capa i p else p) Prepizza

aptaIntolerantesLactosa' :: Pizza -> Bool
aptaIntolerantesLactosa' = foldP (\i b -> not (esQueso i) && b) True

cantidadDeQueso' :: Pizza -> Int
cantidadDeQueso' = foldP (\i n -> if esQueso i then 1 + n else n) 0

conElDobleDeAceitunas' :: Pizza -> Pizza
-- conElDobleDeAceitunas' = foldP (\i p -> Capa (duplicarAceitunas i) p) Prepizza
conElDobleDeAceitunas' = foldP (Capa . duplicarAceitunas) Prepizza

-----------------------------------------------------------------------------------

cantidadAceitunas :: Pizza -> Int 
cantidadAceitunas = foldP (\i n -> nroAceitunas i + n) 0
                          
nroAceitunas :: Ingrediente -> Int
nroAceitunas (Aceitunas n) = n
nroAceitunas m             = 0

capasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
capasQueCumplen f = foldP (\i igs -> if f i then i : igs else igs) []

conDescripcionMejorada :: Pizza -> Pizza 
conDescripcionMejorada = foldP mejorarDescripcion Prepizza

mejorarDescripcion :: Ingrediente -> Pizza -> Pizza
mejorarDescripcion (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n + m)) p
mejorarDescripcion i p = Capa i p

-- , que agrega las capas de la primera pizza sobre la segunda
conCapasDe :: Pizza -> Pizza -> Pizza
conCapasDe = flip (foldP Capa) 

primerasNCapas :: Int -> Pizza -> Pizza
primerasNCapas = flip (foldP (\i p n -> if n == 0 then Prepizza else Capa i (p (n - 1)) )
                       (const Prepizza))

--------------------------------------------------------------------------


sum :: [Int] -> Int 
-- sum = foldr (\x n -> x + n) 0
sum = foldr (+) 0

length :: [a] -> Int
-- length = foldr (\x n -> 1 + n) 0
length = foldr (const (1+)) 0

map :: (a -> b) -> [a] -> [b] 
map = flip (foldr (\x xs f -> f x : xs f) (const []))

filter :: (a -> Bool) -> [a] -> [a]
filter = flip (foldr (\x xs p -> if p x then x : xs p else xs p) (const []))

find :: (a -> Bool) -> [a] -> Maybe a 
find = flip (foldr (\x m p -> if p x then Just x else m p ) (const Nothing))

any :: (a -> Bool) -> [a] -> Bool 
any = flip (foldr (\x b p -> p x || b p ) (const False))

all :: (a -> Bool) -> [a] -> Bool 
all = flip (foldr (\x b p -> p x && b p) (const True))

countBy :: (a -> Bool) -> [a] -> Int
countBy = flip (foldr (\x n p -> if p x then 1 + n p else n p) (const 0))

partition :: (a -> Bool) -> [a] -> ([a], [a]) 
-- partition p = foldr (\x par -> if p x then ( x : fst par, snd par ) else (fst par, x : snd par)) ([],[])
partition = flip (foldr (\x par p -> if p x then ( x : fst (par p), snd (par p) ) else (fst (par p), x : snd (par p))) (const ([],[])))

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = flip (foldr (\x xs f ys -> case ys of
                                [] -> []
                                (y:ys') -> f x y : xs f ys') (\_ _ -> []))

-- scanr :: (a -> b -> b) -> b -> [a] -> [b] 

-- takeWhile :: (a -> Bool) -> [a] -> [a]

-- take :: Int -> [a] -> [a] 

-- drop :: Int -> [a] -> [a] 

-- elemAt :: Int -> [a] -> a