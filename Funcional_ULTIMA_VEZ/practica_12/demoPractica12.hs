Demostrar la siguiente propiedad:

para todo x. has (==x) = any (elem x) . objectsPerLevel
para todo x. para todo m. has (==x) mapa = (any (elem x) . objectsPerLevel) mapa ?
por definicion de (.)
para todo x. para todo m. has (==x) mapa = any (elem x) (objectsPerLevel mapa) ?
sea e un elemento cualquiera y mapa de tipo Mapa a, por ppio de induccion sobre la estructura de mapa, es equivalente demostrar que:

Caso Base : mapa = Cofre xs

has (==x) (Cofre xs) = any (elem x) (objectsPerLevel (Cofre xs)) ?

Caso inductivo 1: mapa = Nada m')

HI) has (==x) m' = any (elem x) (objectsPerLevel m') !
TI) has (==x) (Nada m') = any (elem x) (objectsPerLevel (Nada m')) ?

Caso inductivo 2: mapa = Bifurcacion xs m1 m2)

HI1) has (==x) m1 = any (elem x) (objectsPerLevel m1) !
HI2) has (==x) m2 = any (elem x) (objectsPerLevel m2) !
TI) has (==x) (Bifurcacion xs m1 m2) = any (elem x) (objectsPerLevel (Bifurcacion xs m1 m2)) !

Caso Base:

LI)

has (==x) (Cofre xs)
--------------------
=                   def has
any f



LD)

any (elem x) (objectsPerLevel (Cofre xs))
             ----------------------------
=                   def objectsPerLevel
any (elem x) [xs]
-----------------
=                   def

