data Gusto = Chocolate | DulceDeLeche | Frutilla | Sambayon deriving(Show)

data Helado = Vasito Gusto | Cucurucho Gusto Gusto | Pote Gusto Gusto Gusto deriving(Show)

chocoHelate :: (Gusto -> Helado) -> Helado
chocoHelate consH = consH Chocolate

swap (x,y) = (y,x)

compose f g x = f (g x)

-- 1

-- a. Vasito :: Gusto -> Helado

-- b. Chocolate :: Gusto

-- c. Cucurucho :: Gusto -> Gusto -> Helado

-- d. Sambayon :: Gusto

-- e. Pote :: Gusto -> Gusto -> Gusto -> Helado

-- f. chocoHelate :: (Gusto -> Helado) -> Helado

-- g. chocoHelate Vasito :: Helado

-- h. chocoHelate Cucurucho :: NO TIPA

-- i. chocoHelate (Cucurucho Sambayon) :: Helado

-- j. chocoHelate (chocoHelate Cucurucho) :: No TIPA

-- k. chocoHelate (Vasito DulceDeLeche) :: No tipa

-- l. chocoHelate Pote :: No TIPA

-- m. chocoHelate (chocoHelate (Pote Frutilla)) :: No tipa


-- 2. Dado el siguiente tipo que pretende representar dígitos binarios

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

-- 3. Dado el siguiente tipo que pretende representar dígitos decimales

data DigDec = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9

-- a. , que dado un símbolo que representa un dígito decimal lo transforma en su significado como número.
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

-- b. , que dado un número entre 0 y 9 lo transforma en el símbolo que representa a ese dígito.
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
prevDD D1 = D8
prevDD D2 = D7
prevDD D3 = D6
prevDD D4 = D5
prevDD D5 = D4
prevDD D6 = D3
prevDD D7 = D2
prevDD D8 = D1
prevDD D9 = D0

-- Ejercicio 4

data Medida = Mm Float | Cm Float | Inch Float | Foot Float deriving (Show)

-- que dada una medida cualquiera la transforma en una medida en milímetros que aproxima la dada según la conversión establecida
asMm :: Medida -> Medida
asMm (Mm m)   = Mm m
asMm (Cm m)   = Cm (m * 0.1)
asMm (Inch m) = Inch (m * 0.039)
asMm (Foot m) = Foot (m * 0.003)

-- que dada una medida cualquiera la transforma en una medida en centímetros que aproxima la dada según la conversión establecida
asCm :: Medida -> Medida
asCm (Mm m)   = Mm (m * 10)
asCm (Cm m)   = Cm m
asCm (Inch m) = Inch (m * 0.394)
asCm (Foot m) = Foot (m * 0.033)

asInch :: Medida -> Medida
asInch (Mm m)   = Mm (m * 25.4)
asInch (Cm m)   = Cm (m * 2.54) 
asInch (Inch m) = Inch m
asInch (Foot m) = Foot (m * 0.083)

asFoot :: Medida -> Medida
asFoot (Mm m)   = Mm (m * 304.8)
asFoot (Cm m)   = Cm (m * 30.48)
asFoot (Inch m) = Inch (m * 12)
asFoot (Foot m) = Foot m 

-- Ejercicio 5

data Shape = Circle Float | Rect Float Float deriving(Show)

construyeShNormal :: (Float -> Shape) -> Shape
construyeShNormal c = c 1.0


-- a. uncurry Rect :: (Float, Float) -> Shape

exp1 = uncurry Rect (1.0,2.0)

-- b. construyeShNormal (flip Rect 5.0) :: Shape

-- c. compose (uncurry Rect) swap :: (Float, Float) -> Shape

exp2 = compose (uncurry Rect) swap (3.0, 4.0)

-- d. uncurry Cucurucho :: (Gusto, Gusto) -> Helado

exp3 = uncurry Cucurucho (Frutilla, Chocolate)

-- e. uncurry Rect swap :: No tiene tipo

-- f. compose uncurry Pote :: Gusto -> (Gusto, Gusto) -> Helado

exp4 = compose uncurry Pote Frutilla (Frutilla, DulceDeLeche)

-- g. compose Just :: (a -> a) -> a -> Maybe a

-- exp5 = compose Just Cucurucho Frutilla

-- h. compose uncurry (Pote Chocolate) :: NO TIENE TIPO

-- Ejercicio 7

data Set a = S (a -> Bool) 

belongs :: Set a -> a -> Bool
belongs (S f) x = f x

empty :: Set a
empty = S (\x -> False)

singleton :: Eq a => a -> Set a
singleton e = S (\x -> x == e)

union :: Set a -> Set a -> Set a
union (S f) (S g) = S (\x -> f x || g x)

intersection :: Set a -> Set a -> Set a
intersection (S f) (S g) = S (\x -> f x && g x)

-- Ejercicio 8

data MayFail a = Raise Exception | Ok a

data Exception = DivByZero | NotFound | NullPointer | Other String

type ExHandler a = Exception -> a


-- que dada una computación que puede fallar (MayFail a), una función que indica cómo continuar si no falla (a -> b),
--  y un manejador de los casos de falla ExHandler b, expresa la computación completa
tryCatch :: MayFail a -> (a -> b) -> ExHandler b -> b
tryCatch (Raise e) f handler = handler e
tryCatch (Ok a) f handler = f a

-- sueldoGUIE :: Nombre -> [Empleado] -> GUI Int
-- sueldoGUIE nombre empleados = 
--     tryCatch (lookupE nombre empleados)
--             mostrarInt
--             (\e -> case e of
--                     NotFound -> ventanaError msgNotEmployee
--                     _        -> error msgUnexpected)
--     where msgNotEmployee = "No es empleado de la empresa"
--           msgUnexpected  = "Error inesperado"

-- mostrarInt :: Int -> GUI Int
-- ventanaError :: String -> GUI a
-- lookupE :: Nombre -> [Empleado] -> MayFail Int

compose12 :: (d -> c) -> (a -> b -> d) -> a -> b -> c
compose12 f g x y = f (g x y)

twice :: (a -> a) -> a -> a
twice f x = f (f x)
                             -- f           g         x  
-- compose12 twice compose :: (b -> c) -> (a -> b) -> a -> c

funcionA = \f g x -> f (g (f (g x)))