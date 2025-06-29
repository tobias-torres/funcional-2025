## Demostraciones 
## Ejercicio 6


a) para todo f. length . capasQueCumplen f = cantidadDe f ?
para todo f. para todo p. (length . capasQueCumplen) f p = cantidadDe f p
por def de (.)
para todo f. para todo p. length (capasQueCumplen f) p = cantidadDe f p ?
sea pz de tipo Pizza, por ppio de induccion sobre la estructura de pz, es equivalente demostrar que:

CASO BASE: pz = Prepizza

length (capasQueCumplen f) Prepizza = cantidadDe f Prepizza ?

CASO INDUCTIVO: pz = (Capa i pizza)

HI) length (capasQueCumplen f) pizza = cantidadDe f pizza !
TI) length (capasQueCumplen f) (Capa i pizza) = cantidadDe f (Capa i pizza) ?



b. para todo f. para todo p1. para todo p2. cantidadCapasQueCumplen f (conCapasDe p1 p2) = cantidadCapasQueCumplen f p1 + cantidadCapasQueCumplen f p2 ?
sean f' de tipo funcion, pz1 y pz2 de tipo pizza. por ppio de induccion sobre la estructura de pz1, es equivalente demostrar que:

CASO BASE: pz1 = Prepizza

cantidadCapasQueCumplen f (conCapasDe Prepizza pz2) = cantidadCapasQueCumplen f Prepizza + cantidadCapasQueCumplen f pz2 ?

CASO INDUCTIVO: pz1 = Capa i p)

HI) cantidadCapasQueCumplen f (conCapasDe p pz2) = cantidadCapasQueCumplen f p + cantidadCapasQueCumplen f pz2 !
TI) cantidadCapasQueCumplen f (conCapasDe (Capa i p) pz2) = cantidadCapasQueCumplen f (Capa i p) + cantidadCapasQueCumplen f pz2 ?

CASO BASE:

LI)

cantidadCapasQueCumplen f (conCapasDe Prepizza pz2)
                            ----------------------
=               conCapasDe
cantidadCapasQueCumplen f pz2

LD)

cantidadCapasQueCumplen f Prepizza + cantidadCapasQueCumplen f pz2
----------------------------------
=               def cantidadCapasQueCumplen
0 + cantidadCapasQueCumplen f pz2
---------------------------------
=               0 es neutro en la suma
cantidadCapasQueCumplen f pz2


CASO INDUCTIVO:

LI)

cantidadCapasQueCumplen f (conCapasDe (Capa i p) pz2)
                            ------------------------
=               def conCapasDe
cantidadCapasQueCumplen f (Capa i (conCapasDe p pz2))
----------------------------------------------------
=               def cantidadCapasQueCumplen
sumaSiCumple f i + cantidadCapasQueCumplen f (conCapasDe p pz2)
                    ------------------------------------------
=               HI
sumaSiCumple f i + cantidadCapasQueCumplen f p + cantidadCapasQueCumplen f pz2

LD)

cantidadCapasQueCumplen f (Capa i p) + cantidadCapasQueCumplen f pz2
-----------------------------------
=               def cantidadCapasQueCumplen
sumaSiCumple f i + cantidadCapasQueCumplen f p + cantidadCapasQueCumplen f pz2

-- c
para todo f. para todo p1. para todo p2. conCapasTransformadas f (conCapasDe p1 p2) = conCapasDe (conCapasTransformadas f p1) (conCapasTransformadas f p2) ?
sea f' de tipo funcion, pz1 y pz2 de tipo Pizza. Por ppio de induccion sobre la estructura de pz1, es equivalente demostrar que:

CASO BASE: pz1 = Prepizza

conCapasTransformadas f' (conCapasDe Prepizza p2) = conCapasDe (conCapasTransformadas f' Prepizza) (conCapasTransformadas f' p2) ?

CASO INDUCTIVO: pz1 = Capa i p)

HI) conCapasTransformadas f' (conCapasDe p pz2) = conCapasDe (conCapasTransformadas f' p) (conCapasTransformadas f' pz2) !
TI) conCapasTransformadas f' (conCapasDe (Capa i p) pz2) = conCapasDe (conCapasTransformadas f' (Capa i p)) (conCapasTransformadas f' pz2) ?

CASO BASE:

LI)

conCapasTransformadas f' (conCapasDe Prepizza p2) 
                        -------------------------
=               def conCapasDe
conCapasTransformadas f' pz2

LD)

conCapasDe (conCapasTransformadas f' Prepizza) (conCapasTransformadas f' p2) 
            ---------------------------------
=               def conCapasTransformadas
conCapasDe Prepizza (conCapasTransformadas f' p2)
-------------------------------------------------
=               def conCapasDe
conCapasTransformadas f' p2

CASO INDUCTIVO

LI)

conCapasTransformadas f' (conCapasDe (Capa i p) pz2) 
                         ---------------------------
=               def conCapasDe
conCapasTransformadas f' (Capa i (conCapasDe p pz2))
----------------------------------------------------
=               def conCapasTransformadas
Capa (f' i) (conCapasTransformadas f' (conCapasDe p pz2))

LD)

conCapasDe (conCapasTransformadas f' (Capa i p)) (conCapasTransformadas f' pz2) 
            -----------------------------------
=               def conCapasTransformadas
conCapasDe (Capa (f' i) (conCapasTransformadas f' p)) (conCapasTransformadas f' pz2) 
------------------------------------------------------------------------------------
=               def conCapasDe
Capa (f' i) (conCapasDe (conCapasTransformadas f' p) (conCapasTransformadas f' pz2))
            ------------------------------------------------------------------------
=               HI
Capa (f' i) (conCapasTransformadas f' (conCapasDe p pz2))



d. para todo f. cantidadCapasQueCumplen f . soloLasCapasQue f = cantidadCapasQueCumplen f ?
para todo f. para todo p. (cantidadCapasQueCumplen f . soloLasCapasQue f) p = cantidadCapasQueCumplen f p ?
por def de (.)
para todo f. para todo p. cantidadCapasQueCumplen f (soloLasCapasQue f p) = cantidadCapasQueCumplen f p ?
sea f' de tipo funcion, pz de tipo Pizza. Por ppio de induccion sobre la estructura de pz, es equivalente demostrar que:

CASO BASE: pz = Prepizza

cantidadCapasQueCumplen f (soloLasCapasQue f Prepizza) = cantidadCapasQueCumplen f Prepizza ?

CASO INDUCTIVO: pz = Capa i pizza)

HI) cantidadCapasQueCumplen f (soloLasCapasQue f pizza) = cantidadCapasQueCumplen f pizza !
TI) cantidadCapasQueCumplen f (soloLasCapasQue f (Capa i pizza)) = cantidadCapasQueCumplen f (Capa i pizza) ?

CASO BASE:

LI)

cantidadCapasQueCumplen f (soloLasCapasQue f Prepizza)
                            -------------------------
=               def soloLasCapasQue
cantidadCapasQueCumplen f Prepizza

LD)

cantidadCapasQueCumplen f Prepizza

CASO INDUCTIVO:

LI)

cantidadCapasQueCumplen f (soloLasCapasQue f (Capa i pizza))
                            -------------------------------
=               def soloLasCapasQue
cantidadCapasQueCumplen f (armarSiCumple p i (soloLasCapasQue p pz))

LD)

cantidadCapasQueCumplen f (Capa i pizza)
----------------------------------------
=               def cantidadCapasQueCumplen
sumaSiCumple f i + cantidadCapasQueCumplen f pizza



soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue p Prepizza    = Prepizza
soloLasCapasQue p (Capa i pz) = armarSiCumple p i (soloLasCapasQue p pz)

armarSiCumple :: (Ingrediente -> Bool) -> Ingrediente -> Pizza -> Pizza
armarSiCumple f i pz = if f i then Capa i pz else pz