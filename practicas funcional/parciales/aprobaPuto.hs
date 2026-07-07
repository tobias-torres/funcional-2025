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
nd2nb []   = []
nd2nb ndec = int2db (evalND ndec)

int2db :: Int -> NBin
int2db 0 = []
int2db n = if mod n 2 == 0 
            then O : int2db (div n 2)
            else I : int2db (div n 2)

-- type NBin = [DigBin]

-- data DigBin = O | I deriving (Show)

-- nb2nd :: NBin -> NDec
-- nb2nd 

