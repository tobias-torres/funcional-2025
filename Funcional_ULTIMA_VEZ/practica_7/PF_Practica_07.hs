data Pizza = Prepizza | Capa Ingrediente Pizza deriving(Show)

data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamon | Queso | Salsa deriving (Show)

pizzaPodrida = Capa Jamon(Capa Queso Prepizza)
pizzaConAceitunas = Capa (Aceitunas 10)(Capa Queso (Capa (Aceitunas 2) Prepizza))
pizzaDos = Capa Queso(Capa (Aceitunas 2) Prepizza)

-- Prepizza pertenece al conjunto de pizza
-- si p es una Pizza e i es un Ingrediente, entonces (Capa i p) es una Pizza 

-- f Prepizza   = ...
-- f (Capa i p) = ... f p ...

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

cantidadDeAceitunas :: Pizza -> Int
cantidadDeAceitunas Prepizza   = 0 
cantidadDeAceitunas (Capa i p) = cuentaAceituna i + cantidadDeAceitunas p 

cuentaAceituna :: Ingrediente -> Int
cuentaAceituna (Aceitunas n) = n
cuentaAceituna _             = 0

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza   = Prepizza
duplicarAceitunas (Capa i p) = Capa (aumentarCantidadAceitunas i) (duplicarAceitunas p) 

aumentarCantidadAceitunas :: Ingrediente -> Ingrediente
aumentarCantidadAceitunas (Aceitunas n ) = Aceitunas (2 * n)
aumentarCantidadAceitunas i              = i

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _     = False

sinLactosa' :: Pizza -> Pizza
sinLactosa' Prepizza   = Prepizza
sinLactosa' (Capa i p) = sinQueso i (sinLactosa' p)

sinQueso :: Ingrediente -> Pizza -> Pizza
sinQueso Queso p = p
sinQueso i p = Capa i p

-- aptaIntolerantesLactosa :: Pizza -> Bool
-- aptaIntolerantesLactosa Prepizza   = True
-- aptaIntolerantesLactosa (Capa i p) = ((&&) . not . esQueso) i (aptaIntolerantesLactosa p) 
-- not (esQueso i) && (aptaIntolerantesLactosa p)

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada Prepizza   = Prepizza
conDescripcionMejorada (Capa i p) = juntarCapas i (conDescripcionMejorada p)

juntarCapas :: Ingrediente -> Pizza -> Pizza
juntarCapas (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n + m)) p
juntarCapas i p = Capa i p

----------------------------------------------------------------------------------------------------

type Nombre = String 

data Planilla = Fin | Registro Nombre Planilla deriving(Show)

data Equipo = Becario Nombre | Investigador Nombre Equipo Equipo Equipo deriving(Show)

-- Fin pertenece al conjunto Planilla
-- si n es un nombre y p es una planilla, entonces (Registro n p) pertenece a Planilla

-- si n es un nombre, entonces (Becario n) pertenece a equipo
-- si n es un nombre, e1, e2, e3 son un equipo, entonces (Investigador n e1 e2 e3) pertenece a Equipo

-- f Fin            = ...
-- f (Registro n p) = ... f p ...

-- f (Becario n)               = ... 
-- f (Investigador n e1 e2 e3) = f e1 ... f e2 ... f e3

planillaUno = Registro "Tobias"(Registro "Lio"(Registro "Manu"
                (Registro "Puri"(Registro "Mini"(Registro "Papi"(Registro "John"
                (Registro "Rocko"(Registro "Manchis"(Registro "Chino"(Registro "Meni" Fin))))))))))

planillaDos = Registro "Coca"(Registro "Nelly"(Registro "Hector"(Registro "Tito" Fin)))

p3 = Registro "Berazategui"(Registro "Quilmes" Fin)

p4 = Registro "Varela" Fin

p1 = Registro "Ana" (Registro "Luis" Fin)
p2 = Registro "Sofia" (Registro "Tomas" Fin)

largoDePlanilla :: Planilla -> Int
largoDePlanilla Fin            = 0
largoDePlanilla (Registro n p) = 1 + largoDePlanilla p

esta :: Nombre -> Planilla -> Bool
esta nombreBuscado Fin            = False
esta nombreBuscado (Registro n p) = nombreBuscado == n || (esta nombreBuscado p)

juntarPlanillas :: Planilla -> Planilla -> Planilla
juntarPlanillas Fin p               = p
juntarPlanillas (Registro n1 p1) p2 = Registro n1 (juntarPlanillas p1 p2)

nivelesJerarquicos :: Equipo -> Int
nivelesJerarquicos (Becario n)               = 1
nivelesJerarquicos (Investigador n e1 e2 e3) = 1 + max (nivelesJerarquicos e1) (max (nivelesJerarquicos e2) (nivelesJerarquicos e3)) 

cantidadDeIntegrantes :: Equipo -> Int
cantidadDeIntegrantes (Becario n)               = 1
cantidadDeIntegrantes (Investigador n e1 e2 e3) = 1 + (cantidadDeIntegrantes e1) + (cantidadDeIntegrantes e2) + (cantidadDeIntegrantes e3) 

planillaDeIntegrantes :: Equipo -> Planilla
planillaDeIntegrantes (Becario n)               = Registro n Fin
planillaDeIntegrantes (Investigador n e1 e2 e3) = Registro n (juntarPlanillas (planillaDeIntegrantes e1) (juntarPlanillas (planillaDeIntegrantes e2) (planillaDeIntegrantes e3))) 

---------------------------------------------------------------------------------------------------------------------

data Dungeon a = 
          Habitacion a 
        | Pasaje (Maybe a) (Dungeon a) 
        | Bifurcacion (Maybe a) (Dungeon a) (Dungeon a) 

data Tesoro = Cofre | Oro | Joyas deriving(Eq)

-- 1. si h es una habitacion, entonces Habitacion a pertenece a Dungeon 
-- si p es un pasaje, m es de tipo maybe y d es un dungeon, entonces Pasaje m d pertenece a Dungeon
-- si b es una bifuracion, m es de tipo maybe y d1, d2 son de tipo Dungeon, entonces (Bifurcacion m d1 d2) es de tipo Dungeon

-- 2.

-- f (Habitacion x)        = ...
-- f (Pasaje m d)          = ... f d
-- f (Bifurcacion m d1 d2) = ... (f d1) ... (f d2)

d1 = Bifurcacion Nothing (Pasaje Nothing (Habitacion Joyas)) (Habitacion Joyas)

dungeonL = (Pasaje Nothing (Habitacion Oro))

cantidadDeBifurcaciones :: Dungeon a -> Int
cantidadDeBifurcaciones (Habitacion e)        = 0
cantidadDeBifurcaciones (Pasaje m d)          = (cantidadDeBifurcaciones d)
cantidadDeBifurcaciones (Bifurcacion m d1 d2) = 1 + (cantidadDeBifurcaciones d1) + (cantidadDeBifurcaciones d2)

cantidadDePuntosInteresantes :: Dungeon a -> Int
cantidadDePuntosInteresantes (Habitacion x)        = 1
cantidadDePuntosInteresantes (Pasaje m d)          = analizarPunto m + (cantidadDePuntosInteresantes d)
cantidadDePuntosInteresantes (Bifurcacion m d1 d2) = analizarPunto m + (cantidadDePuntosInteresantes d1) + (cantidadDePuntosInteresantes d2)

analizarPunto :: Maybe a -> Int
analizarPunto (Just x) = 1
analizarPunto Nothing = 0

cantidadDePuntosVacios :: Dungeon a -> Int
cantidadDePuntosVacios (Habitacion x)        = 0
cantidadDePuntosVacios (Pasaje m d)          = analizarPuntoVacio m + (cantidadDePuntosVacios d)
cantidadDePuntosVacios (Bifurcacion m d1 d2) = analizarPuntoVacio m + (cantidadDePuntosVacios d1) + (cantidadDePuntosVacios d2)

analizarPuntoVacio :: Maybe a -> Int
analizarPuntoVacio Nothing  = 1
analizarPuntoVacio (Just x) = 0

cantidadDePuntosCon :: Eq a => a -> Dungeon a -> Int
cantidadDePuntosCon elem (Habitacion x)        = sumarIguales elem x 
cantidadDePuntosCon elem (Pasaje m d)          = sumarSiCumple elem m + (cantidadDePuntosCon elem d)
cantidadDePuntosCon elem (Bifurcacion m d1 d2) = sumarSiCumple elem m + (cantidadDePuntosCon elem d1) + (cantidadDePuntosCon elem d2)

sumarIguales :: Eq a => a -> a -> Int
sumarIguales e1 e2 = if e1 == e2 then 1 else 0 

sumarSiCumple :: Eq a => a -> Maybe a -> Int
sumarSiCumple elem (Just x) = sumarIguales elem x
sumarSiCumple elem Nothing  = 0

esLineal :: Dungeon a -> Bool
esLineal (Habitacion x)        = True
esLineal (Pasaje m d)          = True && esLineal d
esLineal (Bifurcacion m d1 d2) = False

llenoDe :: Eq a => a -> Dungeon a -> Bool
llenoDe elem (Habitacion x)        = elem == x
llenoDe elem (Pasaje m d)          = esElMismoElemento elem m && (llenoDe elem d)
llenoDe elem (Bifurcacion m d1 d2) = esElMismoElemento elem m && (llenoDe elem d1) && (llenoDe elem d2)

esElMismoElemento :: Eq a => a -> Maybe a -> Bool
esElMismoElemento elem (Just x) = elem == x
esElMismoElemento elem Nothing  = False

data VariasCosas a b = Objeto a | Criatura b 

data Monstruo = Gargola | Dragon | Troll
