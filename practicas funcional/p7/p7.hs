data Pizza = Prepizza | Capa Ingrediente Pizza deriving (Show)

data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamón | Queso | Salsa deriving (Show)

-- Prepizza pertenece al conjunto Pizza
-- si p es una pizza e i es un ingrediente, entonces (Capa i p) es una pizza

-- f Prepizza   = ...
-- f (Capa i p) = ... f p

pizzaNapolitana = Capa Queso (Capa (Aceitunas 2) (Capa Salsa (Capa Jamón Prepizza)))
pizzaSinQueso = Capa (Aceitunas 12) (Capa Jamón (Capa Salsa Prepizza))

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

cantidadDeAceitunas :: Pizza -> Int
cantidadDeAceitunas Prepizza   = 0
cantidadDeAceitunas (Capa i p) = nroAceitunas i + cantidadDeAceitunas p

nroAceitunas (Aceitunas n) = n
nroAceitunas _             = 0

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza   = Prepizza
duplicarAceitunas (Capa i p) = Capa (duplicar i) (duplicarAceitunas p)

duplicar (Aceitunas n) = Aceitunas (2 * n)
duplicar i             = i 

sinLactosa :: Pizza -> Pizza
sinLactosa Prepizza   = Prepizza
sinLactosa (Capa i p) = sinQueso i (sinLactosa p)

sinQueso :: Ingrediente -> Pizza -> Pizza
sinQueso Queso p = p
sinQueso i p     = Capa i p

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa Prepizza   = True
aptaIntolerantesLactosa (Capa i p) = not (esQueso i) && aptaIntolerantesLactosa p

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _     = False

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada Prepizza   = Prepizza
conDescripcionMejorada (Capa i p) = mejorar i (conDescripcionMejorada p)

mejorar (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n + m)) p
mejorar i p = Capa i p

-- Demostraciones --

-- a

-- cantidadDeAceitunas Prepizza = cantidadDeAceitunas(conDescripcionMejorada Prepizza)

-- Lado Izq)

-- cantidadDeAceitunas Prepizza

-- Lado Der)

-- cantidadDeAceitunas(conDescripcionMejorada Prepizza)
--                     -------------------------------
-- =                               def conDescripcionMejorada
-- cantidadDeAceitunas Prepizza

-- -- b

-- cantidadDeAceitunas (Capa Queso Prepizza) = cantidadDeAceitunas (conDescripcionMejorada (Capa Queso Prepizza)

-- Lado Izq)

-- cantidadDeAceitunas (Capa Queso Prepizza)

-- Lado Der)

-- cantidadDeAceitunas (conDescripcionMejorada (Capa Queso Prepizza))
--                     ---------------------------------------------
-- =                               def conDescripcionMejorada
-- cantidadDeAceitunas (mejorar Queso (conDescripcionMejorada Prepizza))
--                     ------------------------------------------------
-- =                               def conDescripcionMejorada
-- cantidadDeAceitunas (Capa Queso (conDescripcionMejorada Prepizza))
--                                 ---------------------------------
-- =                               def conDescripcionMejorada
-- cantidadDeAceitunas (Capa Queso Prepizza)

-- c

-- cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza)) = cantidadDeAceitunas 
--       (conDescripcionMejorada  
--          (Capa (Aceitunas 8) 
--                (Capa Queso Prepizza)))

-- Lado Izq)

-- cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza))
-- -------------------------------------------------------------
-- =                               def de cantidadDeAceitunas
-- nroAceitunas (Aceitunas 8) + cantidadDeAceitunas (Capa Queso Prepizza)
-- --------------------------
-- =                               def de nroAceitunas
-- 8 + cantidadDeAceitunas (Capa Queso Prepizza)
--     -----------------------------------------
-- =                               def de cantidadDeAceitunas
-- 8 + nroAceitunas Queso + cantidadDeAceitunas Prepizza

-- Lado Der)

-- cantidadDeAceitunas (conDescripcionMejorada (Capa (Aceitunas 8) (Capa Queso Prepizza)))
--                      -----------------------------------------------------------------
-- =                               def conDescripcionMejorada
-- cantidadDeAceitunas ( mejorar (Aceitunas 8) (conDescripcionMejorada (Capa Queso Prepizza)))
--                     ----------------------------------------------------------------------
-- =                               def mejorar
-- cantidadDeAceitunas ( Capa (Aceitunas 8) (conDescripcionMejorada (Capa Queso Prepizza)))
--                                           --------------------------------------------
-- =                               def conDescripcionMejorada
-- cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza))
-- -------------------------------------------------------------
-- =                               def cantidadDeAceitunas
-- nroAceitunas (Aceitunas 8) + cantidadDeAceitunas (Capa Queso Prepizza)
-- --------------------------
-- =                               def nroAceitunas
-- 8 + cantidadDeAceitunas (Capa Queso Prepizza)
--     -----------------------------------------
-- =                               def cantidadDeAceitunas
-- 8 + cantidadDeAceitunas Queso + cantidadDeAceitunas Prepizza

-- d

-- cantidadDeAceitunas (Capa (Aceitunas 9)  
--                           (Capa (Aceitunas 8) 
--                                 (Capa Queso Prepizza))) = cantidadDeAceitunas (conDescripcionMejorada  
--          (Capa (Aceitunas 9)  
--                (Capa (Aceitunas 8) 
--                      (Capa Queso Prepizza))))

-- Lado Izq)

-- cantidadDeAceitunas (Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza)))
-- -----------------------------------------------------------------------------------
-- =                               def cantidadDeAceitunas
-- nroAceitunas (Aceitunas 9) + cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza))
-- --------------------------
-- =                               def nroAceitunas
-- 9 + cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza))
--     --------------------------------------------------------------
-- =                               def cantidadDeAceitunas
-- 9 + nroAceitunas (Aceitunas 8) + cantidadDeAceitunas (Capa Queso Prepizza)
--     --------------------------
-- =                               def nroAceitunas
-- 9 + 8 + cantidadDeAceitunas (Capa Queso Prepizza)
--         -----------------------------------------
-- =                               def de cantidadDeAceitunas
-- 9 + 8 + nroAceitunas Queso + cantidadDeAceitunas Prepizza
--         ------------------
-- =                               def de nroAceitunas
-- 9 + 8 + 0 + cantidadDeAceitunas Prepizza
--             ----------------------------
-- =                               def cantidadDeAceitunas Prepizza
-- 9 + 8 + 0 + 0
-- -------------
-- =                               def suma
-- 17

-- Lado Der)

-- cantidadDeAceitunas (conDescripcionMejorada (Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza))))
--                     ----------------------------------------------------------------------------------------
-- =                               def conDescripcionMejorada
-- cantidadDeAceitunas (mejorar (Aceitunas 9) (conDescripcionMejorada (Capa (Aceitunas 8) (Capa Queso Prepizza))))
--                     -----------------------------------------------------------------------------------------
-- =                               def mejorar
-- cantidadDeAceitunas (Capa (Aceitunas (8 + 9)) (conDescripcionMejorada (Capa Queso Prepizza)))
--                                                 ----------------------------------------------------------------
-- =                               def conDescripcionMejorada
-- cantidadDeAceitunas (Capa (Aceitunas (8 + 9)) (Capa Queso Prepizza))
-- --------------------------------------------------------------------
-- =                               def cantidadDeAceitunas
-- nroAceitunas (Aceitunas 17) + cantidadDeAceitunas (Capa Queso Prepizza)
-- ---------------------------
-- =                               def nroAceitunas
-- 17 + cantidadDeAceitunas (Capa Queso Prepizza)
--      -----------------------------------------
-- =                               def cantidadDeAceitunas
-- 17 + nroAceitunas Queso + cantidadDeAceitunas Prepizza
--     -------------------
-- =                               def nroAceitunas
-- 17 + 0 + cantidadDeAceitunas Prepizza
--          ----------------------------
-- =                               def de cantidadDeAceitunas
-- 17 + 0 + 0
-- -----------
-- =                               def suma
-- 17



--------------------------------------------------------------- SECCION II ---------------------------------------------------------------

type Nombre = String 

data Planilla = Fin | Registro Nombre Planilla deriving (Show)

data Equipo = Becario Nombre | Investigador Nombre Equipo Equipo Equipo deriving (Show)


-- Fin pertenece al conjunto Planilla
-- si n es un Nombre y p es una Planilla, entonces (Registro n p) pertenece a una Planilla

-- si n es un Nombre, entonces (Becario n) es un equipo.
-- si n es un Nombre y e1, e2, e3 son Equipo, entonces (Investigador n e1 e2 e3) pertenece a un Equipo.

-- f Fin            = ...
-- f (Registro n p) = ... f p

-- f (Becario n)               = ...
-- f (Investigador n e1 e2 e3) = ... f e1 ... f e2 ... f e3


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

-- d. nivelesJerarquicos, que describe la cantidad de niveles jerárquicos de un equipo dado.
nivelesJerarquicos :: Equipo -> Int
nivelesJerarquicos (Becario n)               = 0
nivelesJerarquicos (Investigador n e1 e2 e3) = 1 + nivelesJerarquicos e1 + nivelesJerarquicos e2 + nivelesJerarquicos e3

-- e. cantidadDeIntegrantes, que describe la cantidad de integrantes de un equipo dado.
cantidadDeIntegrantes :: Equipo -> Int
cantidadDeIntegrantes (Becario n)               = 1
cantidadDeIntegrantes (Investigador n e1 e2 e3) = 1 + cantidadDeIntegrantes e1 + cantidadDeIntegrantes e2 + cantidadDeIntegrantes e3

-- f. planillaDeIntegrantes, que describe la planilla de integrantes de un equipo dado.
planillaDeIntegrantes :: Equipo -> Planilla
planillaDeIntegrantes (Becario n)               = Registro n Fin
planillaDeIntegrantes (Investigador n e1 e2 e3) = Registro n (juntarPlanillas (planillaDeIntegrantes e1) (juntarPlanillas (planillaDeIntegrantes e2) (planillaDeIntegrantes e3)))

--------------------------------------------------------------- SECCION III ---------------------------------------------------------------


data Dungeon a = 
          Habitacion a 
        | Pasaje (Maybe a) (Dungeon a) 
        | Bifurcacion (Maybe a) (Dungeon a) (Dungeon a)

-- si h es una Habitacion, entonces pertenece al conjunto Dungeon a
-- si m es de tipo Maybe a y d es un Dungeon a, entonces Pasaje m d pertenece al conjunto Dungeon a
-- si m es de tipo Maybe a, d1 y d2 son tipo Dungeon a, entonces Bifurcacion m d1 d2 pertenece al conjunto Dungeon a

-- f (Habitacion a)        = 
-- f (Pasaje m d)          = ... f d
-- f (Bifurcacion m d1 d2) = ... f d1 ... f d2

miDungeon :: Dungeon String
miDungeon = 
    Bifurcacion Nothing 
        -- CAMINO IZQUIERDO:
        (Pasaje (Just "Poción de Vida") 
            (Habitacion "Cofre del Tesoro"))
            
        -- CAMINO DERECHO:
        (Bifurcacion (Just "Llave Maestra") 
            -- Sub-camino izquierdo de la rama derecha:
            (Habitacion "Jefe Final") 
            -- Sub-camino derecho de la rama derecha:
            (Pasaje Nothing 
                (Habitacion "Salida del Calabozo")))

cantidadDeBifurcaciones :: Dungeon a -> Int
cantidadDeBifurcaciones (Habitacion a)        = 0
cantidadDeBifurcaciones (Pasaje m d )         = cantidadDeBifurcaciones d
cantidadDeBifurcaciones (Bifurcacion m d1 d2) = 1 + cantidadDeBifurcaciones d1 + cantidadDeBifurcaciones d2

cantidadDePuntosInteresantes :: Dungeon a -> Int
cantidadDePuntosInteresantes (Habitacion a)        = 1
cantidadDePuntosInteresantes (Pasaje m d)          = analizarPuntoInteresante m + cantidadDePuntosInteresantes d
cantidadDePuntosInteresantes (Bifurcacion m d1 d2) = analizarPuntoInteresante m + cantidadDePuntosInteresantes d1 + cantidadDePuntosInteresantes d2

analizarPuntoInteresante :: Maybe a -> Int
analizarPuntoInteresante (Just x) = 1
analizarPuntoInteresante Nothing  = 0

cantidadDePuntosVacios :: Dungeon a -> Int
cantidadDePuntosVacios (Habitacion a)        = 0
cantidadDePuntosVacios (Pasaje m d)          = analizarPuntoVacio m + cantidadDePuntosVacios d
cantidadDePuntosVacios (Bifurcacion m d1 d2) = analizarPuntoVacio m + cantidadDePuntosVacios d1 + cantidadDePuntosVacios d2

analizarPuntoVacio :: Maybe a -> Int
analizarPuntoVacio Nothing  = 1
analizarPuntoVacio (Just x) = 0

cantidadDePuntosCon :: Eq a => a -> Dungeon a -> Int
cantidadDePuntosCon x (Habitacion a)        = analizar x a
cantidadDePuntosCon x (Pasaje m d)          = sumarSiSonElMismo x m + cantidadDePuntosCon x d
cantidadDePuntosCon x (Bifurcacion m d1 d2) = sumarSiSonElMismo x m + cantidadDePuntosCon x d1 + cantidadDePuntosCon x d2

analizar :: Eq a => a -> a -> Int
analizar x x' =  if x == x' then 1 else 0

sumarSiSonElMismo :: Eq a => a -> Maybe a -> Int
sumarSiSonElMismo x (Just x') = analizar x x'
sumarSiSonElMismo x Nothing   = 0

esLineal :: Dungeon a -> Bool
esLineal (Habitacion a)        = True
esLineal (Pasaje m d)          = True && esLineal d
esLineal (Bifurcacion m d1 d2) = False

llenoDe :: Eq a => a -> Dungeon a -> Bool
llenoDe x (Habitacion a)        = x == a
llenoDe x (Pasaje m d)          = esElMismo x m && llenoDe x d
llenoDe x (Bifurcacion m d1 d2) = esElMismo x m && llenoDe x d1 && llenoDe x d2

esElMismo :: Eq a => a -> Maybe a -> Bool
esElMismo x (Just x') = x == x'
esElMismo x Nothing   = False

cantidadDePuntosVacios :: Dungeon a -> Int
cantidadDePuntosVacios (Habitacion a)        = 0
cantidadDePuntosVacios (Pasaje m d)          = analizarPuntoVacio m + cantidadDePuntosVacios d
cantidadDePuntosVacios (Bifurcacion m d1 d2) = analizarPuntoVacio m + cantidadDePuntosVacios d1 + cantidadDePuntosVacios d2

analizarPuntoVacio :: Maybe a -> Int
analizarPuntoVacio Nothing  = 1
analizarPuntoVacio (Just x) = 0

e. para todo z :: a . para todo y :: a . para todo x :: a .
cantidadDePuntosVacios 
  (Bifurcacion Nothing 
     (Pasaje Nothing  (Habitacion z)) 
     (Pasaje (Just y) (Habitacion x))) = 2

Lado Izq)

cantidadDePuntosVacios (Bifurcacion Nothing (Pasaje Nothing  (Habitacion z)) (Pasaje (Just y) (Habitacion x)))
--------------------------------------------------------------------------------------------------------------
=                                       def cantidadDePuntosVacios
analizarPuntoVacio Nothing + cantidadDePuntosVacios (Pasaje Nothing  (Habitacion z)) + cantidadDePuntosVacios (Pasaje (Just y) (Habitacion x))
--------------------------
=                                       def analizarPuntoVacio
1 + cantidadDePuntosVacios (Pasaje Nothing  (Habitacion z)) + cantidadDePuntosVacios (Pasaje (Just y) (Habitacion x))
    -------------------------------------------------------
=                                       def cantidadDePuntosVacios
1 + analizarPuntoVacio Nothing + cantidadDePuntosVacios (Habitacion z) + cantidadDePuntosVacios (Pasaje (Just y) (Habitacion x))
    --------------------------
=                                       def analizarPuntoVacio
1 + 1 + cantidadDePuntosVacios (Habitacion z) + cantidadDePuntosVacios (Pasaje (Just y) (Habitacion x))
        -------------------------------------
=                                       def cantidadDePuntosVacios
1 + 1 + 0 + cantidadDePuntosVacios (Pasaje (Just y) (Habitacion x))
            -------------------------------------------------------
=                                       def cantidadDePuntosVacios
1 + 1 + 0 + analizarPuntoVacio (Just y) + cantidadDePuntosVacios (Habitacion x)
            ---------------------------
=                                       def analizarPuntoVacio
1 + 1 + 0 + 0 + cantidadDePuntosVacios (Habitacion x)
                -------------------------------------
=                                       def cantidadDePuntosVacios
1 + 1 + 0 + 0 + 0
-----------------
=                                       def de +
2

cantidadDePuntosCon :: Eq a => a -> Dungeon a -> Int
cantidadDePuntosCon x (Habitacion a)        = analizar x a
cantidadDePuntosCon x (Pasaje m d)          = sumarSiSonElMismo x m + cantidadDePuntosCon x d
cantidadDePuntosCon x (Bifurcacion m d1 d2) = sumarSiSonElMismo x m + cantidadDePuntosCon x d1 + cantidadDePuntosCon x d2

analizar :: Eq a => a -> a -> Int
analizar x x' =  if x == x' then 1 else 0

sumarSiSonElMismo :: Eq a => a -> Maybe a -> Int
sumarSiSonElMismo x (Just x') = analizar x x'
sumarSiSonElMismo x Nothing   = 0

data VariasCosas a b = Objeto a | Criatura b 

data Monstruo = Gargola | Dragon | Troll

cantidadDePuntosCon (Criatura Troll)
  (Habitacion (Objeto Oro)) = 0 

Lado Izq)

cantidadDePuntosCon (Criatura Troll) (Habitacion (Objeto Oro)) 
--------------------------------------------------------------
=                                           def de cantidadDePuntosCon
analizar (Criatura Troll) (Objeto Oro)
--------------------------------------
=                                           def de analizar
if (Criatura Troll) == (Objeto Oro) then 1 else 0
    -------------------------------
=                                           def de ==
0
