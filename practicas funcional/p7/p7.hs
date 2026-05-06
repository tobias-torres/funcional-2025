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

cantidadDeAceitunas Prepizza = cantidadDeAceitunas(conDescripcionMejorada Prepizza)

Lado Izq)

cantidadDeAceitunas Prepizza

Lado Der)

cantidadDeAceitunas(conDescripcionMejorada Prepizza)
                    -------------------------------
=                               def conDescripcionMejorada
cantidadDeAceitunas Prepizza

-- b

cantidadDeAceitunas (Capa Queso Prepizza) = cantidadDeAceitunas (conDescripcionMejorada (Capa Queso Prepizza)

Lado Izq)

cantidadDeAceitunas (Capa Queso Prepizza)

Lado Der)

cantidadDeAceitunas (conDescripcionMejorada (Capa Queso Prepizza))
                    ---------------------------------------------
=                               def conDescripcionMejorada
cantidadDeAceitunas (mejorar Queso (conDescripcionMejorada Prepizza))
                    ------------------------------------------------
=                               def conDescripcionMejorada
cantidadDeAceitunas (Capa Queso (conDescripcionMejorada Prepizza))
                                ---------------------------------
=                               def conDescripcionMejorada
cantidadDeAceitunas (Capa Queso Prepizza)

-- c

cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza)) = cantidadDeAceitunas 
      (conDescripcionMejorada  
         (Capa (Aceitunas 8) 
               (Capa Queso Prepizza)))

Lado Izq)

cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza))
-------------------------------------------------------------
=                               def de cantidadDeAceitunas
nroAceitunas (Aceitunas 8) + cantidadDeAceitunas (Capa Queso Prepizza)
--------------------------
=                               def de nroAceitunas
8 + cantidadDeAceitunas (Capa Queso Prepizza)
    -----------------------------------------
=                               def de cantidadDeAceitunas
8 + nroAceitunas Queso + cantidadDeAceitunas Prepizza

Lado Der)

cantidadDeAceitunas (conDescripcionMejorada (Capa (Aceitunas 8) (Capa Queso Prepizza)))
                     -----------------------------------------------------------------
=                               def conDescripcionMejorada
cantidadDeAceitunas ( mejorar (Aceitunas 8) (conDescripcionMejorada (Capa Queso Prepizza)))
                    ----------------------------------------------------------------------
=                               def mejorar
cantidadDeAceitunas ( Capa (Aceitunas 8) (conDescripcionMejorada (Capa Queso Prepizza)))
                                          --------------------------------------------
=                               def conDescripcionMejorada
cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza))
-------------------------------------------------------------
=                               def cantidadDeAceitunas
nroAceitunas (Aceitunas 8) + cantidadDeAceitunas (Capa Queso Prepizza)
--------------------------
=                               def nroAceitunas
8 + cantidadDeAceitunas (Capa Queso Prepizza)
    -----------------------------------------
=                               def cantidadDeAceitunas
8 + cantidadDeAceitunas Queso + cantidadDeAceitunas Prepizza

-- d

cantidadDeAceitunas (Capa (Aceitunas 9)  
                          (Capa (Aceitunas 8) 
                                (Capa Queso Prepizza))) = cantidadDeAceitunas (conDescripcionMejorada  
         (Capa (Aceitunas 9)  
               (Capa (Aceitunas 8) 
                     (Capa Queso Prepizza))))

Lado Izq)

cantidadDeAceitunas (Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza)))
-----------------------------------------------------------------------------------
=                               def cantidadDeAceitunas
nroAceitunas (Aceitunas 9) + cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza))
--------------------------
=                               def nroAceitunas
9 + cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza))
    --------------------------------------------------------------
=                               def cantidadDeAceitunas
9 + nroAceitunas (Aceitunas 8) + cantidadDeAceitunas (Capa Queso Prepizza)
    --------------------------
=                               def nroAceitunas
9 + 8 + cantidadDeAceitunas (Capa Queso Prepizza)
        -----------------------------------------
=                               def de cantidadDeAceitunas
9 + 8 + nroAceitunas Queso + cantidadDeAceitunas Prepizza
        ------------------
=                               def de nroAceitunas
9 + 8 + 0 + cantidadDeAceitunas Prepizza
            ----------------------------
=                               def cantidadDeAceitunas Prepizza
9 + 8 + 0 + 0
-------------
=                               def suma
17

Lado Der)

cantidadDeAceitunas (conDescripcionMejorada (Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza))))
                    ----------------------------------------------------------------------------------------
=                               def conDescripcionMejorada
cantidadDeAceitunas (mejorar (Aceitunas 9) (conDescripcionMejorada (Capa (Aceitunas 8) (Capa Queso Prepizza))))
                    -----------------------------------------------------------------------------------------
=                               def mejorar
cantidadDeAceitunas (Capa (Aceitunas (8 + 9)) (conDescripcionMejorada (Capa Queso Prepizza)))
                                                ----------------------------------------------------------------
=                               def conDescripcionMejorada
cantidadDeAceitunas (Capa (Aceitunas (8 + 9)) (Capa Queso Prepizza))
--------------------------------------------------------------------
=                               def cantidadDeAceitunas
nroAceitunas (Aceitunas 17) + cantidadDeAceitunas (Capa Queso Prepizza)
---------------------------
=                               def nroAceitunas
17 + cantidadDeAceitunas (Capa Queso Prepizza)
     -----------------------------------------
=                               def cantidadDeAceitunas
17 + nroAceitunas Queso + cantidadDeAceitunas Prepizza
    -------------------
=                               def nroAceitunas
17 + 0 + cantidadDeAceitunas Prepizza
         ----------------------------
=                               def de cantidadDeAceitunas
17 + 0 + 0
-----------
=                               def suma
17



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


