data Pizza = Prepizza | Capa Ingrediente Pizza deriving (Show)

data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamón | Queso | Salsa deriving (Show)

pz = Capa Salsa (Capa Queso (Capa Anchoas Prepizza))

cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int 
cantidadCapasQueCumplen f Prepizza   = 0
cantidadCapasQueCumplen f (Capa i p) = sumaSiCumple f i + cantidadCapasQueCumplen f p

sumaSiCumple :: (Ingrediente -> Bool) -> Ingrediente -> Int
sumaSiCumple f i = if f i then 1 else 0

conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas f Prepizza   = Prepizza
conCapasTransformadas f (Capa i p) = Capa (f i) (conCapasTransformadas f p)

soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue f Prepizza   = Prepizza
soloLasCapasQue f (Capa i p) = armarSiCumple f i (soloLasCapasQue f p)

armarSiCumple :: (Ingrediente -> Bool) -> Ingrediente -> Pizza -> Pizza
armarSiCumple f i p = if f i then Capa i p else p

-- Ejercicio 2
sinLactosa :: Pizza -> Pizza 
sinLactosa = soloLasCapasQue (not . esQueso)


-- compose :: (b -> c) -> (a -> b) -> a -> c
-- not     :: Bool -> Bool
-- -----------------------------------------
-- compose not :: (a -> Bool) -> a -> Bool
-- esQueso     :: Ingrediente -> Bool
-- -----------------------------------------
-- (not . esQueso) :: Ingrediente -> Bool

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _     = False

aptaIntolerantesLactosa :: Pizza -> Bool 
aptaIntolerantesLactosa = (==0) . cantidadCapasQueCumplen esQueso

cantidadDeQueso :: Pizza -> Int 
cantidadDeQueso = cantidadCapasQueCumplen esQueso

conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas = conCapasTransformadas duplicar

duplicar :: Ingrediente -> Ingrediente
duplicar (Aceitunas n) = Aceitunas (2 * n)
duplicar i             = i 

-- Ejercicio 3

pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b
pizzaProcesada f z Prepizza   = z
pizzaProcesada f z (Capa i p) = f i (pizzaProcesada f z p)

cantidadCapasQueCumplen' :: (Ingrediente -> Bool) -> Pizza -> Int
-- cantidadCapasQueCumplen' f = pizzaProcesada (\i n -> sumaSiCumple f i + n) 0
-- cantidadCapasQueCumplen' f = pizzaProcesada (\i n -> (+) (sumaSiCumple f i) n) 0
-- cantidadCapasQueCumplen' f = pizzaProcesada (\i -> (+) (sumaSiCumple f i)) 0
cantidadCapasQueCumplen' f = pizzaProcesada ((+) . sumaSiCumple f) 0

conCapasTransformadas' :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
-- conCapasTransformadas' f = pizzaProcesada (\i p -> Capa (f i) p) Prepizza
-- conCapasTransformadas' f = pizzaProcesada (\i -> Capa (f i)) Prepizza
conCapasTransformadas' f = pizzaProcesada (Capa . f ) Prepizza


-- soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
-- sinLactosa :: Pizza -> Pizza 
-- aptaIntolerantesLactosa :: Pizza -> Bool 
-- cantidadDeQueso :: Pizza -> Int 
-- conElDobleDeAceitunas :: Pizza -> Pizza

