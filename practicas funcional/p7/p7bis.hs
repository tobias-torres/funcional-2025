data Pizza = Prepizza | Capa Ingrediente Pizza

data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamon | Queso | Salsa

-- 1) Prepizza pertenece al conjunto Pizza,
--    si p es una pizza e i es un ingrediente, entonces (Capa i p) es una pizza

-- 2) 
-- f Prepizza   = ...
-- f (Capa i p) = ... f p

-- a. cantidadDeCapas, que describe la cantidad de capas de ingredientes de la misma.
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

-- b. cantidadDeAceitunas, que describe la cantidad de aceitunas que hay en una pizza dada.
cantidadDeAceitunas :: Pizza -> Int
cantidadDeAceitunas Prepizza   = 0
cantidadDeAceitunas (Capa i p) = sumarAceituna i + cantidadDeAceitunas p

sumarAceituna :: Ingrediente -> Int
sumarAceituna (Aceitunas n) = n
sumarAceituna _             = 0

-- c. duplicarAceitunas, que dada una pizza, describe otra pizza de forma tal que se cumpla la siguiente propiedad:
-- para todo p. cantidadDeAceitunas (duplicarAceitunas p) = 2 * cantidadDeAceitunas p
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza   = Prepizza
duplicarAceitunas (Capa i p) = Capa (duplicar i) (duplicarAceitunas p)

duplicar :: Ingrediente -> Ingrediente
duplicar (Aceitunas n ) = (Aceitunas (2 * n))
duplicar i              = i

-- d. sinLactosa, que describe la pizza resultante de remover todas las capas de queso de una pizza dada.
sinLactosa :: Pizza -> Pizza
sinLactosa Prepizza   = Prepizza
sinLactosa (Capa i p) = removerQueso i (sinLactosa p)

removerQueso :: Ingrediente -> Pizza -> Pizza
removerQueso Queso p = p
removerQueso i p     = Capa i p

-- e. aptaIntolerantesLactosa, que indica si la pizza dada no tiene queso, o sea se cumple la siguiente propiedad: para todo p. si ​aptaIntolerantesLactosa ​p​ ​=​ True
-- entonces ​p​ ​=​ sinLactosa ​p
aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa Prepizza   = True
aptaIntolerantesLactosa (Capa i p) = not (esQueso i) && aptaIntolerantesLactosa p

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _     = False

-- que toma una pizza y otra que se construyó con exactamente los mismos ingredientes pero donde no se agregan aceitunas dos veces seguidas.
conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada Prepizza   = Prepizza
conDescripcionMejorada (Capa i p) = mejorar i (conDescripcionMejorada p)

mejorar :: Ingrediente -> Pizza -> Pizza
mejorar (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n+m)) p
mejorar i p                                  = Capa i p


type Nombre = String

data Planilla = Fin | Registro Nombre Planilla

data Equipo = Becario Nombre | Investigador Nombre Equipo Equipo Equipo

-- 1 
-- Fin pertenece al conjunto Planilla
-- si n es un Nombre y p es una Planilla, entonces (Registro n p) pertenece a una Planilla

-- si n es un Nombre, entonces (Becario n) es un equipo.
-- si n es un Nombre y e1, e2, e3 son Equipo, entonces (Investigador n e1 e2 e3) pertenece a un Equipo.

-- f Fin            = ...
-- f (Registro n p) = ... f p

-- f (Becario n)               = ...
-- f (Investigador n e1 e2 e3) = ... f e1 ... f e2 ... f e3

-- a. , que describe la cantidad de nombres en una planilla dada.
largoDePlanilla :: Planilla -> Int
largoDePlanilla Fin            = 0
largoDePlanilla (Registro n p) = 1 + largoDePlanilla p

-- b. esta, que toma un nombre y una planilla e indica si en la planilla dada está el nombre dado.
esta :: Nombre -> Planilla -> Bool
esta n Fin             = False
esta n (Registro n' p) = n == n' || esta n p

-- c. juntarPlanillas, que toma dos planillas y genera una única planilla con los registros de ambas planillas.
juntarPlanillas :: Planilla -> Planilla -> Planilla
juntarPlanillas Fin p             = p
juntarPlanillas (Registro n p) p2 = Registro n (juntarPlanillas p p2)

-- d. nivelesJerarquicos, que describe la cantidad de niveles jerárquicos de
-- un equipo dado.
nivelesJerarquicos :: Equipo -> Int
nivelesJerarquicos (Becario n)               = 0
nivelesJerarquicos (Investigador n e1 e2 e3) = 1 + nivelesJerarquicos e1 + nivelesJerarquicos e2 + nivelesJerarquicos e3

-- e. cantidadDeIntegrantes, que describe la cantidad de integrantes de un
-- equipo dado.
cantidadDeIntegrantes :: Equipo -> Int
cantidadDeIntegrantes (Becario n)               = 1
cantidadDeIntegrantes (Investigador n e1 e2 e3) = 1 + cantidadDeIntegrantes e1 + cantidadDeIntegrantes e2 + cantidadDeIntegrantes e3

-- f. planillaDeIntegrantes, que describe la planilla de integrantes de un equipo dado.
planillaDeIntegrantes :: Equipo -> Planilla
planillaDeIntegrantes (Becario n)               = Fin
planillaDeIntegrantes (Investigador n e1 e2 e3) = Registro n (juntarPlanillas (planillaDeIntegrantes e1) (juntarPlanillas (planillaDeIntegrantes e2) (planillaDeIntegrantes e3)))

