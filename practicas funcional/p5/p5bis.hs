data Gusto = Chocolate | DulceDeLeche | Frutilla | Sambayon

data Helado = Vasito Gusto | Cucurucho Gusto Gusto | Pote Gusto Gusto Gusto


-- chocoHelate consH = consH Chocolate

-- chocoHelate       :: (Gusto -> a) -> a
-- consH             :: Gusto -> a
-- ----------------------------------
-- chocoHelate consH :: a


-- consH     :: Gusto -> a
-- Chocolate :: Gusto
-- -------------------------
-- consH Chocolate :: a

-- chocoHelate :: (Gusto -> a) -> a
-- Cucurucho   :: Gusto -> Gusto -> Helado
-- -----------------------------------------   a <- Gusto -> Helado
-- chocoHelate Cucurucho :: Gusto -> Helado


-- a. Vasito :: Gusto -> Helado
-- b. Chocolate :: Gusto
-- c. Cucurucho :: Gusto -> Gusto -> Helado
-- d. Sambayon :: Gusto
-- e. Pote :: Gusto -> Gusto -> Gusto -> Helado
-- f. chocoHelate :: (Gusto -> a) -> a
-- g. chocoHelate Vasito :: Helado
-- h. chocoHelate Cucurucho :: Gusto -> Helado
-- i. chocoHelate (Cucurucho Sambayon) :: Helado
-- j. chocoHelate (chocoHelate Cucurucho) :: Helado
-- k. chocoHelate (Vasito DulceDeLeche) :: NO TIPA
-- l. chocoHelate Pote :: Gusto -> Gusto -> Helado
-- m. chocoHelate (chocoHelate (Pote Frutilla)) :: Helado

-- data Shape = Circle Float | Rect Float Float

-- construyeShNormal :: (Float -> Shape) -> Shape
-- construyeShNormal c = c 1.0

-- curry :: ((a,b) -> c) -> a -> b -> c 
-- curry f x y = f (x,y)

-- uncurry :: (a -> b -> c) -> (a,b) -> c 
-- uncurry f (x, y) = f x y

-- Determinar el tipo de las siguientes expresiones:

-- a. uncurry Rect

-- uncurry :: (a -> b -> c) -> (a,b) -> c 
-- Rect    :: Float -> Float -> Shape
-- --------------------------------------
-- uncurry Rect :: (Float, Float) -> Shape

-- b. construyeShNormal (flip Rect 5.0)

-- construyeShNormal                 :: (Float -> Shape) -> Shape
-- flip Rect 5.0                     :: Float -> Shape
-- -----------------------------------------------
-- construyeShNormal (flip Rect 5.0) :: Shape

-- flip          :: (a -> b -> c) -> (b -> a -> c)
-- Rect          :: Float -> Float -> Shape
-- -----------------------------------------------
-- flip Rect     :: Float -> Float -> Shape
-- 5.0           :: Float
-- -----------------------------------------------
-- flip Rect 5.0 :: Float -> Shape


-- c. compose (uncurry Rect) swap

-- compose        :: (b -> c) -> (a -> b) -> a -> c
-- (uncurry Rect) :: (Float, Float) -> Shape
-- ------------------------------------------------ b <- (Float, Float), c <- Shape
-- compose (uncurry Rect) :: (a -> (Float, Float)) -> a -> Shape
-- swap                   :: (a2, b2) -> (b2, a2)
-- ------------------------------------------------ a <- (a2, b2), (Float, Float) <- (b2, a2)
-- compose (uncurry Rect) swap :: (Float, Float) -> Shape


-- d. uncurry Cucurucho :: (Gusto, Gusto) -> Helado

-- e. uncurry Rect swap :: no tipa

-- f. compose uncurry Pote

-- compose              :: (b1 -> c1) -> (a1 -> b1) -> a1 -> c1
-- uncurry              :: (a2 -> b2 -> c2) -> (a2, b2) -> c2
-- ------------------------------------------------------------ b1 <- (a2 -> b2 -> c2), c1 <- (a2, b2) -> c2
-- compose uncurry      :: (a1 -> a2 -> b2 -> c2) -> a1 -> (a2, b2) -> c2
-- Pote                 :: Gusto -> Gusto -> Gusto -> Helado
-- ------------------------------------------------------------ a1 <- Gusto, a2 <- Gusto, b2 <- Gusto, c2 <- Helado
-- compose uncurry Pote :: Gusto -> (Gusto, Gusto) -> Helado

-- g. compose Just

-- compose :: (b1 -> c1) -> (a1 -> b1) -> a1 -> c1
-- Just    ::  a2 -> Maybe a
-- ----------------------------------------------- b1 <- a2, c1 <- Maybe c1
-- compose Just :: (a1 -> a2) -> a1 -> Maybe c1

-- h. compose uncurry (Pote Chocolate)

-- compose          :: (b1 -> c1) -> (a1 -> b1) -> a1 -> c1
-- uncurry          :: (a2 -> b2 -> c2) -> (a2, b2) -> c2
-- ------------------------------------------------------- b1 <- (a2 -> b2 -> c2), c1 <- (a2, b2) -> c2
-- compose uncurry  :: (a1 -> a2 -> b2 -> c2) -> a1 -> (a2, b2) -> c2
-- (Pote Chocolate) :: Gusto -> Gusto -> Helado

-- no tiene tipo

-- 6)

-- a. uncurry Rect 


-- b. construyeShNormal (flip Rect 5.0)
-- c. compose (uncurry Rect) swap
-- d. uncurry Cucurucho
-- e. uncurry Rect swap
-- f. compose uncurry Pote
-- g. compose Just
-- h. compose uncurry (Pote Chocolate)

-- a.

-- uncurry Rect :: (Float, Float) -> Shape

-- uncurry Rect (1.0, 2.0)

-- b.

-- construyeShNormal (flip Rect 5.0) :: Shape

-- c.

-- flip Rect 5.0 :: Float -> Shape

-- flip Rect 5.0 5.0

-- d.

-- compose (uncurry Rect) swap :: (Float, Float) -> Shape

-- compose (uncurry Rect) swap (2.0, 3.0)

-- e.

-- uncurry Cucurucho :: (Gusto, Gusto) -> Helado

-- uncurry Cucurucho (Chocolate, Frutilla)

-- f.

-- compose uncurry Pote :: Gusto -> (Gusto, Gusto) -> Helado

-- compose uncurry Pote Chocolate (Frutilla, Sambayon)

-- compose Just :: (a1 -> a2) -> a1 -> Maybe a

-- 7)

data Set a = S (a -> Bool) 

mayores_Diez_ = S (\x -> x > 10)

-- a. , que dado un conjunto, describe la función que indica si un elemento dado pertenece a ese conjunto.
belongs :: Set a -> a -> Bool
belongs (S p) x = p x

-- b. , que describe el conjunto vacío.
empty :: Set a
empty = S (\x -> False)

-- c. , que dado un elemento describe un conjunto que contiene a ese único elemento.
singleton :: Eq a => a -> Set a
singleton x = S (\x' -> x == x')

-- d. , que dados dos conjuntos, describe al conjunto que resulta de la unión de ambos.
union :: Set a -> Set a -> Set a
union (S p) (S p') = S (\e -> p e || p' e)

-- e. , que dados dos conjuntos, describe al conjunto que resulta de la intersección de ambos.
intersection :: Set a -> Set a -> Set a
intersection (S p) (S p') = S (\e -> p e && p' e)

-- 8)

data MayFail a = Raise Exception | Ok a

data Exception = DivByZero | NotFound | NullPointer | Other String

type ExHandler a = Exception -> a

tryCatch :: MayFail a -> (a -> b) -> ExHandler b -> b
tryCatch (Ok e) f exhandler    = f e
tryCatch (Raise e) f exhandler = exhandler e

-- sueldoGUIE :: Nombre -> [Empleado] -> GUI Int
-- sueldoGUIE nombre empleados = 
--     tryCatch (lookupE nombre empleados)
--              mostrarInt
--             (\e -> case e of
--                     NotFound -> ventanaError msgNotEmployee
--                     _        -> error msgUnexpected)
--     where msgNotEmployee = "No es empleado de la empresa"
--           msgUnexpected  = "Error inesperado"

-- mostrarInt :: Int -> GUI Int
-- ventanaError :: String -> GUI a
-- lookupE :: Nombre -> [Empleado] -> MayFail Int

