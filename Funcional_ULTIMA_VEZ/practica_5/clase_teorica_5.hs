-- Dados los siguientes tipos y funciones:

data Gusto = Chocolate | DulceDeLeche | Frutilla | Sambayon deriving(Show)

data Helado = Vasito Gusto | Cucurucho Gusto Gusto | Pote Gusto Gusto Gusto deriving(Show)

data Shape = Circle Float | Rect Float Float deriving (Show)

cuadrado :: Float -> Shape
cuadrado n = Rect n n

shapeNormal :: (Float -> a) -> a
shapeNormal c = c 1

-- shapeNormal :: (Float -> a) -> a              c :: Float -> a
-- c           ::  Float -> a                    1 :: Float
-- -----------------------------------           ----------------------
-- shapeNormal c :: a                            c 1 :: a

chocoHelate :: (Gusto -> a) -> a
chocoHelate c = c Chocolate


data Par a = MKP a a

string2Gusto "DDL" = DulceDeLeche

string2Gusto "Chocolate" = Chocolate

string2Gusto "Frutilla" = Frutilla

-- armarHeladoCon :: 
armarHeladoCon sg = Vasito (sg "Chocolate")

-- string2Gusto :: String -> a

-- 1
-- Dar el tipo de las siguientes expresiones

-- cuadrado :: Float -> Shape 

-- shapeNormal :: (Float -> a) -> a

-- chocoHelate :: (Gusto -> a) -> a

-- 2

-- shapeNormal Circle :: Shape
-- shapeNormal Circle
-- ------------------
-- =                       def de shapeNormal, c -> Circle
-- Circle 1


-- -- shapeNormal cuadrado :: Shape
-- shapeNormal cuadrado
-- --------------------
-- =                       def de shapeNormal, c -> cuadrado
-- cuadrado 1
-- ----------
-- =                       def de cuadrado, n -> 1
-- Rect 1 1


-- -- shapeNormal Rect :: Float -> Shape

-- shapeNormal Rect

-- no hay redex


-- -- shapeNormal Rect 2 :: Shape
-- shapeNormal Rect 2
-- ----------------
-- =                       def de shapeNormal, c -> Rect
-- Rect 1 2


-- -- shapeNormal (Rect 2) :: Shape
-- shapeNormal (Rect 2)
-- --------------------
-- =                       def de shapeNormal, c -> Rect
-- Rect 2 1


-- -- chocoHelate Vasito :: Helado
-- chocoHelate Vasito
-- ------------------
-- =                       def de chocoHelate, c -> Vasito
-- Vasito Chocolate


-- -- chocoHelate Cucurucho :: Gusto -> Helado
-- chocoHelate Cucurucho
-- ---------------------
-- =                       def de chocoHelate, c -> Cucurucho
-- Cucurucho Chocolate


-- -- chocoHelate Cucurucho Sambayon :: Helado
-- chocoHelate Cucurucho Sambayon
-- ---------------------
-- =                       def de chocoHelate, c -> Cucurucho
-- Cucurucho Chocolate Sambayon


-- -- chocoHelate (Cucurucho Sambayon) :: Helado
-- chocoHelate (Cucurucho Sambayon)
-- --------------------------------
-- =                       def de chocoHelate, c -> (Cucurucho Sambayon)
-- Cucurucho Sambayon Chocolate


-- -- chocoHelate (\g -> Cucurucho g Sambayon) :: Helado
-- chocoHelate (\g -> Cucurucho g Sambayon)
-- ----------------------------------------
-- =                       def de chocoHelate, c -> (\g -> Cucurucho g Sambayon)
-- (\g -> Cucurucho g Sambayon) Chocolate
-- ----------------------------
-- =                       beta reduccion, g -> Chocolate
-- Cucurucho Chocolate Sambayon


-- -- chocoHelate (chocoHelate Cucurucho) :: Helado
-- chocoHelate (chocoHelate Cucurucho)
--             -----------------------
-- =                                           def de chocoHelate, c -> Cucurucho
-- chocoHelate (Cucurucho Chocolate)
-- ---------------------------------
-- =                                           def de chocoHelate, c' -> (Cucurucho Chocolate)
-- Cucurucho Chocolate Chocolate


-- -- chocoHelate (flip Cucurucho Sambayon) :: Helado
-- chocoHelate (flip Cucurucho Sambayon)
-- -------------------------------------
-- =                                           def de chocoHelate, f -> (flip Cucurucho Sambayon)
-- flip Cucurucho Sambayon Chocolate
-- -----------------------------------
-- =                                           def de flip, f -> Cucurucho, x -> Sambayon, y -> Chocolate
-- Cucurucho Chocolate Sambayon


-- -- chocoHelate Pote :: Gusto -> Gusto -> Helado
-- chocoHelate Pote
-- ----------------
-- =                                           def de chocoHelate, f -> Pote
-- Pote Chocolate

-- no hay mas redex


-- -- chocoHelate poteDeUnGusto :: Gusto -> Helado
-- chocoHelate poteDeUnGusto
-- -------------------------
-- =                                           def de chocoHelate, f -> poteDeUnGusto
-- poteDeUnGusto Chocolate
-- -------------------------
-- =                                           def de poteDeUnGusto
-- Pote Chocolate


-- -- armarHeladoCon string2Gusto :: Helado
-- armarHeladoCon string2Gusto
-- ---------------------------
-- =                                           def de armarHeladoCon
-- Vasito (string2Gusto "Chocolate")
--         -------------------------
-- =                                           def de string2Gusto
-- Vasito Chocolate


-- -- armarHeladoCon error :: Helado
-- armarHeladoCon error
-- --------------------
-- Vasito (error "Chocolate")
--         -----------------
-- =                                               def de error

-- NO LLEGO A LA FORMA NORMAL

-- -- esValido(armarHeladoCon error) :: Bool
-- esValido(armarHeladoCon error)
--          --------------------
-- =                                               def de armarHeladoCon
-- esValido (Vasito (error "Chocolate"))
--                  -------------------
-- =                                               def de error

-- da bottom, no llego a la forma normal

-- 3, escribir funciones

esCuadrado :: Shape -> Bool
esCuadrado (Rect n1 n2) = n1 == n2
esCuadrado _            = False 

siempreArmaCuadradoDeTamanio :: Float -> (Float -> Shape) -> Bool
siempreArmaCuadradoDeTamanio n f = esCuadrado (f n) && (n /= 0)

siempreArmaCuadrado :: (Float -> Shape) -> Bool
siempreArmaCuadrado f = esCuadrado (shapeNormal f)

rec1 n = Rect n 1

rec2 n = Rect 1 n

-- shapeNormal :: (Float -> a) -> a
-- shapeNormal c = c 1

-- 4 , reducir las expresiones

-- siempreArmaCuadrado cuadrado :: Bool
-- siempreArmaCuadrado cuadrado
-- ----------------------------
-- =                                                   def siempreArmaCuadrado, f -> cuadrado
-- esCuadrado (shapeNormal cuadrado)
--             -------------------
-- =                                                   def shapeNormal, c -> cuadrado
-- esCuadrado (cuadrado 1)
--             ----------
-- =                                                   def cuadrado, n -> 1
-- esCuadrado (Rect 1 1)
-- ---------------------
-- =                                                   def esCuadrado
-- 1 == 1
-- ------
-- =                                                   def de ==
-- True


-- -- siempreArmaCuadrado (Rect 2)
-- siempreArmaCuadrado (Rect 2)
-- ----------------------------
-- =                                                   def siempreArmaCuadrado
-- esCuadrado (shapeNormal (Rect 2))
--             --------------------
-- =                                                   def shapeNormal
-- esCuadrado(Rect 2 1)
-- -------------------
-- =                                                   def esCuadrado
-- 2 == 1
-- ------
-- =                                                   def de ==
-- False

-- shapeNormal :: (Float -> a) -> a
-- shapeNormal c = c 1


-- siempreArmaCuadrado (shapeNormal Rect)
-- siempreArmaCuadrado (shapeNormal Rect)
--                     ------------------
-- =                                                       def de shapeNormal, c -> Rect
-- siempreArmaCuadrado (Rect 1)
-- ----------------------------
-- =                                                       def de siempreArmaCuadrado, f -> (Rect 1)
-- esCuadrado (shapeNormal(Rect 1))
--             ------------------
-- =                                                       def de shapeNormal, c -> (Rect 1)
-- esCuadrado (Rect 1 1)
-- ---------------------
-- =                                                       def de esCuadrado
-- True


-- cuales deberian dar True y cuales False, por que? sus definiciones dan lo mismo ?

-- 5 , dar algunos elementos de los siguientes tipos

-- Par Bool
parBool = MKP True False

-- Par Helado
parHeladito = MKP (Vasito Chocolate) (Pote Chocolate Chocolate Frutilla)

-- Par (Gusto -> Helado)
parFuncion = MKP (Cucurucho Frutilla) (Pote Frutilla DulceDeLeche)

-- 6 dar el tipo de


-- MKP (Cucurucho DulceDeLeche) :: Par
-- MKP                      :: a -> (a -> Par a)
-- (Cucurucho DulceDeLeche) :: Gusto -> Helado          a -> (Gusto -> Helado)
-- --------------------------------------------
-- MKP (Cucurucho DulceDeLeche) :: (Gusto -> Helado) -> Par (Gusto -> Helado)