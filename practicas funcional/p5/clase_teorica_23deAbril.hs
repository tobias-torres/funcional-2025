data Par a = DosCosas a a

data Gusto = Chocolate | DulceDeLeche | Frutilla | Sambayon deriving(Show)

data Helado = Vasito Gusto | Cucurucho Gusto Gusto | Pote Gusto Gusto Gusto deriving(Show)

data Shape = Circle Float | Rect Float Float deriving (Show)

cosaUno = DosCosas True True

cosaLoca = DosCosas Vasito (Pote DulceDeLeche Frutilla)

cosaSinTipo = DosCosas (Cucurucho DulceDeLeche) (Cucurucho DulceDeLeche)

shapeNormal :: (Float -> a) -> a
shapeNormal c = c 1

cuadrado :: Float -> Shape
cuadrado n = Rect n n

esCuadrado :: Shape -> Bool
esCuadrado (Rect n1 n2) = n1 == n2
esCuadrado _            = False 

expresion = (\n -> if n > 10 then cuadrado (n+1) else Circle n)

siempreArmaCuadradoDeTamanio :: Float -> (Float -> Shape) -> Bool
siempreArmaCuadradoDeTamanio n f = esCuadrado (f n) && (n /= 0)

armarCuadradoDeTamanioCuandoReciba :: Float -> Float -> (Float -> Shape) -> Bool
armarCuadradoDeTamanioCuandoReciba n m f = esCuadradoDeTamanio n (f m)

esCuadradoDeTamanio :: Float -> Shape -> Bool
esCuadradoDeTamanio n (Rect b a) = b == n && a == n
esCuadradoDeTamanio _  _         = False

siempreArmaCuadrado :: (Float -> Shape) -> Bool
siempreArmaCuadrado f = esCuadrado (shapeNormal f)

--------------------------------------------------------------------------------------------------------

-- reducciones :

shapeNormal Circle
------------------
=                   def de shapeNormal , c <- Circle
Circle 1


shapeNormal cuadrado
--------------------
=                   def de shapeNormal, c <- cuadrado
cuadrado 1 
----------
=                   def de cuadrado, n <- 1
Rect 1 1


shapeNormal Rect
----------------
=                   def de shapeNormal, c <- Rect
Rect 1



shapeNormal Rect 2
----------------
=                   def shapeNormal, c <- Rect 2
Rect 1 2


shapeNormal (Rect 2)
--------------------
=                       def shapeNormal, c <- (Rect 2)
Rect 2 1


chocoHelate Vasito
------------------
=                       def chocoHelate, 