-- Ejercicio 6

-- a. 

-- a) para todo f. length . capasQueCumplen f = cantidadDe f ?
-- para todo f. para todo p. (length . capasQueCumplen) f p = cantidadDe f p
-- por def de (.)
-- para todo f. para todo p. length (capasQueCumplen f) p = cantidadDe f p ?
-- sea pz de tipo Pizza, por ppio de induccion sobre la estructura de pz, es equivalente demostrar que:

-- Caso Base: pz = Prepizza

-- length (capasQueCumplen f) Prepizza = cantidadDe f Prepizza ?

-- Caso Inductivo: pz = Capa i p')

-- HI) length (capasQueCumplen f) p' = cantidadDe f p' ?
-- TI) length (capasQueCumplen f) (Capa i p') = cantidadDe f (Capa i p') ?

-- Caso Base:

-- I)

-- length (capasQueCumplen f) Prepizza
--         ---------------------------
-- =                               def capasQueCumplen
-- length 0

-- D)

-- b

para todo f. para todo p1. para todo p2. cantidadCapasQueCumplen f (conCapasDe p1 p2) = cantidadCapasQueCumplen f p1 +  cantidadCapasQueCumplen f p2 
sea g una funcion de (Ingrediente -> Bool), y pizza1, pizza2 de tipo Pizza. Por ppio de induccion sobre la estructura de pizza1, es equivalente demostrar que:

Caso Base: pizza1 = Prepizza

cantidadCapasQueCumplen g (conCapasDe Prepizza pizza2) = cantidadCapasQueCumplen g Prepizza + cantidadCapasQueCumplen g pizza2 

Caso Inductivo: pizza1 = (Capa i p)

HI) cantidadCapasQueCumplen g (conCapasDe p pizza2) = cantidadCapasQueCumplen g p +  cantidadCapasQueCumplen g pizza2 !
TI) cantidadCapasQueCumplen g (conCapasDe (Capa i p) pizza2) = cantidadCapasQueCumplen g (Capa i p) +  cantidadCapasQueCumplen g pizza2 ?

Caso Base:

I)

cantidadCapasQueCumplen g (conCapasDe Prepizza pizza2)
                            -------------------------
=                           def de conCapasDe
cantidadCapasQueCumplen g pizza2

D)

cantidadCapasQueCumplen g Prepizza +  cantidadCapasQueCumplen g pizza2 
----------------------------------
=                           def de cantidadCapasQueCumplen
0 + cantidadCapasQueCumplen g pizza2 
------------------------------------
=                           def de +
cantidadCapasQueCumplen g pizza2

Caso Inductivo:

I)

cantidadCapasQueCumplen g (conCapasDe (Capa i p) pizza2)
                            ---------------------------
=                           def de conCapasDe
cantidadCapasQueCumplen g (Capa i (conCapasDe p pizza2))
--------------------------------------------------------
=                           def de cantidadCapasQueCumplen
sumaSiCumple g i + cantidadCapasQueCumplen g (conCapasDe p pizza2)
                    ----------------------------------------------
=                           HI
sumaSiCumple g i + cantidadCapasQueCumplen g p +  cantidadCapasQueCumplen g pizza2
D)

cantidadCapasQueCumplen g (Capa i p) +  cantidadCapasQueCumplen g pizza2
------------------------------------
=                           def de cantidadCapasQueCumplen
sumaSiCumple g i + cantidadCapasQueCumplen g p + cantidadCapasQueCumplen g pizza2



-- conCapasDe :: Pizza -> Pizza -> Pizza
-- conCapasDe Prepizza pz   = pz
-- conCapasDe (Capa i p) pz = Capa i (conCapasDe p pz)

conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas f Prepizza   = Prepizza
conCapasTransformadas f (Capa i p) = Capa (f i) (conCapasTransformadas f p)




c.

para todo f. para todo p1. para todo p2. conCapasTransformadas f (conCapasDe p1 p2) = conCapasDe (conCapasTransformadas f p1) (conCapasTransformadas f p2) 
sea g una funcion de (Ingrediente -> Bool), y pizza1, pizza2 de tipo Pizza. Por ppio de induccion sobre la estructura de pizza1, es equivalente demostrar que:

Caso Base: pizza1 = Prepizza

conCapasTransformadas g (conCapasDe Prepizza pizza2) = conCapasDe (conCapasTransformadas g Prepizza) (conCapasTransformadas g pizza2)

Caso Inductivo: pizza1 = (Capa i p)

HI) conCapasTransformadas g (conCapasDe p pizza2) = conCapasDe (conCapasTransformadas g p) (conCapasTransformadas g pizza2) !
TI) conCapasTransformadas g (conCapasDe (Capa i p) pizza2) = conCapasDe (conCapasTransformadas g (Capa i p)) (conCapasTransformadas g pizza2) ?

Caso Base:

I)

conCapasTransformadas g (conCapasDe Prepizza pizza2)
                        ----------------------------
=                                   def de conCapasDe 
conCapasTransformadas g pizza2

D)

conCapasDe (conCapasTransformadas g Prepizza) (conCapasTransformadas g pizza2)
            --------------------------------
=                                   def de conCapasTransformadas
conCapasDe Prepizza (conCapasTransformadas g pizza2)
----------------------------------------------------
=                                   def de conCapasDe
conCapasTransformadas g pizza2

Caso Inductivo: 

I)

conCapasTransformadas g (conCapasDe (Capa i p) pizza2)
                        -----------------------------
=                                   def de conCapasDe
conCapasTransformadas g (Capa i (conCapasDe p pizza2))
------------------------------------------------------
=                                   def de conCapasTransformadas
Capa (g i) conCapasTransformadas g (conCapasDe p pizza2)

D)

conCapasDe (conCapasTransformadas g (Capa i p)) (conCapasTransformadas g pizza2)
            ----------------------------------
=                                   def de conCapasTransformadas
conCapasDe (Capa (g i) (conCapasTransformadas g p)) (conCapasTransformadas g pizza2)
------------------------------------------------------------------------------------
=                                   def de conCapasDe
Capa (g i) (conCapasDe (conCapasTransformadas g p) (conCapasTransformadas g pizza2))
            ------------------------------------------------------------------------
=                                   HI
Capa (g i) conCapasTransformadas g (conCapasDe p pizza2) 

-- conCapasDe :: Pizza -> Pizza -> Pizza
-- conCapasDe Prepizza pz   = pz
-- conCapasDe (Capa i p) pz = Capa i (conCapasDe p pz)

d.  para todo f. cantidadCapasQueCumplen f . soloLasCapasQue f = cantidadCapasQueCumplen f ?
para todo f. para todo p1. (cantidadCapasQueCumplen f . soloLasCapasQue f) p1 = cantidadCapasQueCumplen f p1 ?
por def de (.)
para todo f. para todo p1. cantidadCapasQueCumplen f (soloLasCapasQue f p1) = cantidadCapasQueCumplen f p1 ?
sea g una funcion, pizza de tipo Pizza, por ppio de induccion sobre la estructura de pizza

Caso Base: pizza = Prepizza

cantidadCapasQueCumplen f (soloLasCapasQue f Prepizza) = cantidadCapasQueCumplen f Prepizza ?

Caso Inductivo: pizza = (Capa i p)

HI) cantidadCapasQueCumplen f (soloLasCapasQue f p) = cantidadCapasQueCumplen f p !
TI) cantidadCapasQueCumplen f (soloLasCapasQue f (Capa i p)) = cantidadCapasQueCumplen f (Capa i p) ?

Caso Base: 

I)

cantidadCapasQueCumplen f (soloLasCapasQue f Prepizza)
                            --------------------------
=                           def de soloLasCapasQue
cantidadCapasQueCumplen f Prepizza

D)

cantidadCapasQueCumplen f Prepizza 

Caso Inductivo:

I)

cantidadCapasQueCumplen f (soloLasCapasQue f (Capa i p))
                            ---------------------------
=                           def de soloLasCapasQue
cantidadCapasQueCumplen f (armarSiCumple f i (soloLasCapasQue f p))

D)

cantidadCapasQueCumplen f (Capa i p)
------------------------------------
=                           def de cantidadCapasQueCumplen
sumaSiCumple f i + cantidadCapasQueCumplen f p
                    --------------------------
=                           HI
sumaSiCumple f i + cantidadCapasQueCumplen f (soloLasCapasQue f p)


a.  map id = id
por ppio de extensionalidad 
para todo xs. map id xs = id xs ?
sea ys una lista, por ppio de induccion sobre la estructura de ys, es equivalente demostrar que:

Caso Base: ys = []

map id [] = id [] ?

Caso Inductivo: ys = x:xs')

HI) map id xs' = id xs' ?
TI) map id (x:xs') = id (x:xs') ?

Caso Base:

I)

map id []
---------
=           def de map
[]

D)

id []
-----
=           def de id
[]

Caso Inductivo:

I)

map id (x:xs')
--------------
=           def de map
id x : map id xs'
----
=           def de id
x : map id xs'
    ---------
=           HI
x : xs'

D)

id (x:xs')
----------
=           def de id
(x:xs')

b.  para todo f. para todo g. map f . map g = map (f . g) ?
para todo f. para todo g. para todo xs. (map f . map g) xs = map (f . g) xs ?
por def de (.)
para todo f. para todo g. para todo xs. map f (map g xs) = map f (g xs)?
sea f' y g' dos funciones, sm una lista. Por ppio de induccion sobre la estructura de sm, es equivalente demostrar que:

Caso Base: sm = []

map f' (map g' []) = map f' (g' []) ?

Caso Inductivo: sm = (x:xs')

HI) map f' (map g' xs') = map f' (g' xs') !
TI) map f' (map g' (x:xs')) = map f' (g' (x:xs')) ?


Caso Base:

I)

map f' (map g' [])
       --------
=                   def de map
map f' []
---------
=                   def de map
[]

D)

map f' (g' [])
--------------
=                   def de map
[]

Caso Inductivo:

I)

map f' (map g' (x:xs'))
        -------------
=                   def de map
map f' (g' x : map g' xs')


D)

map f' (g' (x:xs'))



c. 

para todo f. para todo xs. para todo ys. map f (xs ++ ys) = map f xs ++ map f ys ?
sea g una funcion, sm y sn dos listas. Por ppio de induccion sobre la estructura de sm, es equivalente demostrar que:

Caso Base: sm = []

map g ([] ++ sn) = map g [] ++ map g sn ?

Caso Inductivo: sm = x:xs')

HI) map g (xs' ++ sn) = map g xs' ++ map g sn !
TI) map g ((x:xs') ++ sn) = map g (x:xs') ++ map g sn ?

Caso Base:

I)

map g ([] ++ sn)
        -------
=               def de ++
map g sn

D)

map g [] ++ map g sn
--------
=               def de map
[] ++ map g sn
--------------
=               def de ++
map g sn

Caso Inductivo:

I)

map g ((x:xs') ++ sn)
        -----------
=               def de ++
map g (x: (xs' ++ sn))
----------------------
=               def de map
g x : map g (xs' ++ sn)
      -----------------
=               HI
g x : map g xs' ++ map g sn

D)

map g (x:xs') ++ map g sn
------------
=               def de map
g x : (map g xs') ++ map g sn


d.

para todo f. concat . map (map f) = map f . concat ?
para todo f. para todo xss. (concat . map (map f)) xss = (map f . concat) xss ?
por def de (.)
para todo f. para todo xss. concat (map (map f) xss) = map f (concat xss) ?
sea g una funcion, yss una lista de listas. Por ppio de induccion sobre la estructura de yss, es equivalente demostrar que:

Caso Base: yss = []

concat (map (map g) []) = map g (concat []) ?

Caso Inductivo: yss = (xs:xss')

HI) concat (map (map g) xss') = map g (concat xss') !
TI) concat (map (map g) (xs:xss')) = map g (concat (xs:xss')) ?


Caso Base: 

I)

concat (map (map g) [])
        ---------------
=               def de map
concat []
---------
=               def de concat
[]

D)

map g (concat [])
       ----------
=               def de concat
map g []
--------
=               def de map
[]

Caso Inductivo:

I)

concat (map (map g) (xs:xss'))
        ---------------------
=               def de map
concat (map g xs : map (map g) xss')
------------------------------------
=               def de concat
map g xs ++ concat (map (map g) xss')
            -------------------------
=               HI
map g xs ++ map g (concat xss')
-------------------------------
=               por def del ejercicio 8.c, llego a esto
map g (xs ++ concat xss')


D)

map g (concat (xs:xss'))
       ----------------
=               def de concat
map g (xs ++ concat xss')


e.

foldr ((+) . suma') 0 = sum . map suma' 

f.  para todo f. para todo z. foldr f z . foldr (:) [] = foldr f z 

g.  para todo f. para todo z. para todo xs. para todo ys.     foldr f z (xs ++ ys) = foldr f (foldr f z ys) xs 

h.  (+1) . foldr (+) 0 = foldr (+) 1 

i.  para todo n. para todo f. many n f = foldr (.) id (replicate n f) 
siendo many 0 f = id 
      many n f = f . many (n - 1) f  

j.  para todo f. para todo xs. para todo ys.      zipWith (flip f) xs ys  
    = map (uncurry f) (flip zip xs ys)




















