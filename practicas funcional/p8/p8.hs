-- f []     =
-- f (x:xs) = ... f xs
import Prelude hiding ((++), succ, reverse, length, sum, product, concat, elem, all, any, count, subset, zip, unzip)

lista = [[23,33],[1],[45,66]]
listaDos = [1,2,3,4,5,66]

-- a. 
length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs

-- b. 
sum' :: [Int] -> Int
sum' []     = 0
sum' (x:xs) = x + sum' xs

-- c. que describe el producto entre todos los elementos de la lista
product' :: [Int] -> Int
product' []     = 1
product' (x:xs) = x * product' xs

-- -- d. que describe la lista resultante de concatenar todas las listas que son elementos de la dada.
concat' :: [[a]] -> [a]
concat' []       = []
concat' (xs:xss) = xs ++ concat' xss

-- -- e.  que indica si el elemento dado pertenece a la lista.
elem :: Eq a => a -> [a] -> Bool
elem e []     = False
elem e (x:xs) = e == x || elem e xs

-- -- f. , que indica si todos los elementos de la lista cumplen el predicado dado.
all' :: (a -> Bool) -> [a] -> Bool
all' f []     = True
all' f (x:xs) = f x && all' f xs

funcionLoca = \x -> 10 > x

-- -- g. 
-- , que indica si algún elemento de la lista cumple el predicado dado.
any' :: (a -> Bool) -> [a] -> Bool
any' f []     = False
any' f (x:xs) = f x || any' f xs

-- -- h. 
-- , que describe la cantidad de elementos de la lista que cumplen el predicado dado.
count' :: (a -> Bool) -> [a] -> Int
count' f []     = 0
count' f (x:xs) = sumarSiCumple f x + count' f xs

sumarSiCumple :: (a -> Bool) -> a -> Int
sumarSiCumple f x = if f x then 1 else 0

-- -- i. 
--  que indica si todos los elementos de la primera lista se encuentran en la segunda.
subset' :: Eq a => [a] -> [a] -> Bool
subset' [] ys     = True
subset' (x:xs) ys = elem x ys && subset' xs ys

-- -- j. 
-- , que describe el resultado de agregar los elementos de la primera lista adelante de los elementos de la segunda.
(++) :: [a] -> [a] -> [a]
(++) []     ys = ys
(++) (x:xs) ys = x : (xs ++ ys)

-- k. 
-- que describe la lista que tiene los elementos en el orden inverso a la lista dada.
reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

-- -- l. que describe la lista resultante de juntar de a pares los elementos  de  ambas  listas, según  la posición que comparten en cada una
zip :: [a] -> [b] -> [(a,b)]
zip [] ys         = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys

-- -- m. 
-- , que describe el par de listas que
-- resulta de desarmar la lista dada; la primera componente del resultado se
-- corresponde con las primeras componentes de los pares dados, y la segunda
-- componente con las segundas componentes de dichos pares.
unzip :: [(a,b)] -> ([a],[b])
unzip []     = ([],[])
unzip (x:xs) = (fst x : fst (unzip xs), snd x : snd (unzip xs))

--------------------------------------------------------------------------------------------------

data N = Z | S N deriving (Show)

-- pattern matching sobre una sola estructura

uno = S Z
dos = S (S Z)
tres = S (S (S Z))
cuatro = S (S (S (S Z)))
cinco = S (S (S (S (S Z))))
seis =  S (S (S (S (S (S Z)))))

-- que describe el número representado por el elemento dado. 
evalN :: N -> Int
evalN Z     = 0
evalN (S n) = 1 + evalN n

-- , que describe la representación unaria de la suma  de  los  números  representados  por  los  argumentos.  La 
-- resolución  debe  ser  exclusivamente  simbólica, o sea, SIN calcular cuáles son esos números. 
addN :: N -> N -> N
addN Z m     = m
addN (S n) m = S (addN n m)

-- , que describe la representación unaria del producto  de  los  números  representados  por  los  argumentos.  La 
-- resolución debe ser exclusivamente simbólica. 
prodN :: N -> N -> N
prodN Z m     = Z
prodN (S n) m = addN (prodN n m) m 

-- , que describe la representación unaria del número dado usando el tipo N.
int2N  :: Int -> N
int2N 0 = Z
int2N n = S (int2N (n - 1))

--------------------------------------------------------------------------------------------------

type NU = [()] 

nuList = [(),(),()]
nuList2 = [(),(),(),()]

data Unit = Unit 

-- que describe el número representado por el elemento dado. 
evalNU :: NU -> Int
evalNU []     = 0
evalNU (x:xs) = 1 + evalNU xs

-- que describe la representación unaria del 
-- resultado de sumarle uno al número representado por el argumento. 
-- La resolución debe ser exclusivamente simbólica. 
succNU  ::  NU -> NU
succNU nu = ():nu

-- que describe la representación unaria 
-- de la suma de los números representados por los argumentos. La 
-- resolución debe ser exclusivamente simbólica. 
addNU :: NU -> NU -> NU
addNU [] nu      = nu
addNU (n:nu) nu2 = n : addNU nu nu2

-- que describe la representación unaria dada por 
-- el tipo N correspondiente al número representado por el argumento. 
nu2n :: NU -> N
nu2n []     = Z
nu2n (n:nu) = S (nu2n nu)

 
-- que describe la representación unaria dada por 
-- el tipo NU correspondiente al número representado por el argumento. 
n2nu :: N -> NU
n2nu Z     = []
n2nu (S n) = () : (n2nu n)

-- Ejercicio 3

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

succNB :: NBin -> NBin
succNB []     = [I]
succNB (O:nb) = I : nb
succNB (I:nb) = O : succNB nb

addNB :: NBin -> NBin -> NBin
addNB [] m = m
addNB n [] = n
addNB (n:nb) (m:mb) = f n m (addNB nb mb)

f O O n = I : n
f I I n = O : succNB n
f _ _ n = I : n

normalizarNB :: NBin -> NBin
normalizarNB []     = []
normalizarNB (n:nb) = normalizar n (normalizarNB nb)

normalizar :: DigBin -> [DigBin] -> [DigBin]
normalizar O [] = []
normalizar d ds = d : ds

n2nb :: N -> NBin
n2nb Z     = []
n2nb (S n) = succNB (n2nb n)

evalNB :: NBin -> Int 
evalNB []     = 0
evalNB (n:nb) = dbAsInt n + 2 * ( evalNB nb )

nb2n :: NBin -> N
nb2n []     = Z
nb2n (d:nb) = addN (evalD d) (prodN (S (S Z)) (nb2n nb))

evalD O = Z
evalD I = (S Z)

-- Seccion III

data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA

evalExpA :: ExpA -> Int
evalExpA (Cte n)      = n
evalExpA (Suma e1 e2) = evalExpA e1 + evalExpA e2
evalExpA (Prod e1 e2) = evalExpA e1 * evalExpA e2

simplificarExpA  ::  ExpA  ->  ExpA
simplificarExpA (Cte n)      = (Cte n)
simplificarExpA (Suma e1 e2) = simplificarSuma (simplificarExpA expa1) (simplificarExpA expa2)
simplificarExpA (Prod e1 e2) = simplificarProd (simplificarExpA expa1) (simplificarExpA expa2)

simplificarSuma :: ExpA -> ExpA -> ExpA
simplificarSuma (Cte 0) m = m
simplificarSuma n (Cte 0) = n
simplificarSuma n m       = Suma n m

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd (Cte 0) _ = (Cte 0)
simplificarProd _ (Cte 0) = (Cte 0)
simplificarProd (Cte 1) m = m
simplificarProd n (Cte 1) = n
simplificarProd n m       = Prod n m

cantidadDeSumaCero :: ExpA -> Int
cantidadDeSumaCero (Cte n)      = 0
cantidadDeSumaCero (Suma e1 e2) = analizarSumas e1 e2 + cantidadDeSumaCero e1 + cantidadDeSumaCero e2
cantidadDeSumaCero (Prod e1 e2) = cantidadDeSumaCero e1 + cantidadDeSumaCero e2

analizarSumas :: ExpA -> ExpA -> Int
analizarSumas (Cte 0) n = 1
analizarSumas n (Cte 0) = 1
analizarSumas _ _       = 0

data ExpS = CteS N | SumS ExpS ExpS | ProdS ExpS ExpS

evalES :: ExpS -> Int
evalES (CteS n)          = evalN n
evalES (SumS exp1 exp2)  = evalES exp1 + evalES exp2
evalES (ProdS exp1 exp2) = evalES exp1 * evalES exp2

es2ExpA  ::  ExpS  ->  ExpA
es2ExpA (CteS n)          = Cte (evalN n)
es2ExpA (SumS exp1 exp2)  = Suma (es2ExpA exp1) (es2ExpA exp2)
es2ExpA (ProdS exp1 exp2) = Prod (es2ExpA exp1) (es2ExpA exp2)

expA2es  ::  ExpA -> ExpS
expA2es (Cte n)            = CteS (int2N n)
expA2es (Suma expa1 expa2) = SumS (expA2es expa1) (expA2es expa2)
expA2es (Prod expa1 expa2) = ProdS (expA2es expa1) (expA2es expa2)

evalExpA :: ExpA -> Int
evalExpA (Cte n)      = n
evalExpA (Suma e1 e2) = evalExpA e1 + evalExpA e2
evalExpA (Prod e1 e2) = evalExpA e1 * evalExpA e2

