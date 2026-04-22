data Gusto = Chocolate | DulceDeLeche | Frutilla | Sambayon deriving(Show)

data Helado = Vasito Gusto | Cucurucho Gusto Gusto | Pote Gusto Gusto Gusto deriving(Show)

chocoHelate :: (Gusto -> a) -> a
chocoHelate consH = consH Chocolate


-- chocoHelate       :: (Gusto -> a) -> a
-- consH             :: Gusto -> a
-- --------------------------------------------------
-- chocoHelate consH :: a

-- consH           :: Gusto -> a
-- Chocolate       :: Gusto
-- -------------------------------------------------- 
-- consH Chocolate :: a

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

-- k. chocoHelate (Vasito DulceDeLeche) :: No tipa

-- l. chocoHelate Pote :: Gusto -> Gusto -> Helado

-- m. chocoHelate (chocoHelate (Pote Frutilla)) :: Helado

-- Pote     :: Gusto -> Gusto -> Gusto -> Helado
-- Frutilla :: Gusto
-- -----------------------------------------------
-- chocoHelate   :: (Gusto -> a) -> a
-- Pote Frutilla :: Gusto -> Gusto -> Helado
--------------------------------------------------
-- chocoHelate :: Gusto -> Helado

-- chocoHelate' c = c Chocolate

data Shape = Circle Float | Rect Float Float deriving(Show)

construyeShNormal :: (Float -> Shape) -> Shape
construyeShNormal c = c 1.0

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x) 

swap (x, y) = (y, x)

-- -- a. 
-- uncurry      :: (a -> b -> c) -> (a, b) -> c
-- Rect         :: Float -> Float -> Shape
-- ---------------------------------------
-- uncurry Rect :: (Float, Float) -> Shape
expa = uncurry Rect (3.0, 4.0)


-- -- b.
-- construyeShNormal                 :: (Float -> Shape) -> Shape
-- (flip Rect 5.0)                   :: Float -> Shape
-- ----------------------------------------------------------------
-- construyeShNormal (flip Rect 5.0) :: Shape

-- flip      :: (a -> (b -> c)) -> b -> a -> c
-- Rect      :: Float -> Float -> Shape
-- ----------------------------------------------
-- flip Rect :: Float -> Float -> Shape
-- 5.0       :: Float
-- ------------------------------------
-- (flip Rect 5.0) :: Float -> Shape


-- c.
-- compose (uncurry Rect) swap                 
-- compose      :: (b -> c) -> (a -> b) -> a -> c
-- uncurry Rect :: (Float, Float) -> Shape
-- ----------------------------------------------
-- (.) uncurry Rect      :: (a -> (Float, Float)) -> a -> Shape
-- swap                  :: (a2, b2) -> (b2, a2)
-- -------------------------------------------------------- a <-- (a2, b2), (Float, Float) <-- (b2, a2)
-- (.) uncurry Rect swap :: (Float, Float) -> Shape
expc = compose (uncurry Rect) swap (5.0,5.0) 

-- d.
-- uncurry           :: (a -> b -> c) -> (a, b) -> c
-- Cucurucho         :: Gusto -> Gusto -> Helado
-- --------------------------------------------------
-- uncurry Cucurucho :: (Gusto, Gusto) -> Helado
expd = uncurry Cucurucho (Chocolate, DulceDeLeche)

-- -- e.
-- uncurry      :: (a -> b -> c) -> (a, b) -> c
-- Rect         :: Float -> Float -> Shape
-- --------------------------------------------------
-- uncurry Rect :: (Float, Float) -> Shape
-- swap         :: (a1, b1) -> (b1, a1)
-- --------------------------------------------------
-- uncurry Rect swap :: No tipa

-- f. 
-- compose         :: (b1 -> c1) -> (a1 -> b1) -> a1 -> c1 
-- uncurry         :: (a2 -> b2 -> c2) -> (a2, b2) -> c2
-- -------------------------------------------------- 
-- compose uncurry :: (a1 -> a2 -> b2 -> c2) -> a1 -> (a2, b2) -> c2
-- Pote            :: Gusto -> Gusto -> Gusto -> Helado
-- -----------------------------------------------------------------
-- compose uncurry Pote :: Gusto -> (Gusto, Gusto) -> Helado
expf = compose uncurry Pote Chocolate (Chocolate, DulceDeLeche)

-- -- g. 
-- compose      :: (b1 -> c1) -> (a1 -> b1) -> a1 -> c1 
-- Just         :: a -> Maybe a
-- ----------------------------------------------------- b1 <-- a , c1 <-- Maybe a
-- compose Just :: (a1 -> a) -> a1 -> Maybe a

expg = compose Just id 

-- h.
-- compose uncurry  :: (a1 -> a2 -> b2 -> c2) -> a1 -> (a2, b2) -> c2
-- (Pote Chocolate) :: Gusto -> Gusto -> Helado
-- NO TIPA

-- 7

data Set a = S (a -> Bool) 

belongs :: Set a -> a -> Bool
belongs (S f) e = f e

empty :: Set a
empty = S (\x -> False)

singleton :: Eq a => a -> Set a
singleton x = S (\e -> e == x)

union :: Set a -> Set a -> Set a
union (S f) (S g) = S (\e -> f e || g e)

intersection :: Set a -> Set a -> Set a
intersection (S f) (S g) = S (\e -> f e && g e)

-- 8

data MayFail a = Raise Exception | Ok a

data Exception = DivByZero | NotFound | NullPointer | Other String

type ExHandler a = Exception -> a

tryCatch :: MayFail a -> (a -> b) -> ExHandler b -> b
tryCatch (Raise e) f handler = handler e
tryCatch (Ok a) f handler    = f a

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