DEMOSTRACIONES

a. para todo xs. para todo ys. length (xs ++ ys) = length xs + length ys
Dem) sean zs y ws dos listas. Por ppio de induccion sobre la estructura de zs, es equivalente demostrar que:

Caso Base: zs = []

length ([] ++ ws) = length [] + length ws ?

Caso Inductivo: zs = (x:xs')

HI) length (xs' ++ ws) = length xs' + length ws !
TI) length ((x:xs') ++ ws) = length (x:xs') + length ws ?

---

Caso Base

Izq)

length ([] ++ ws)
        --------
=               def de ++
length ws

Der)

length [] + length ws
--------
=              def de length.1
0 + length ws
-------------
=              def aritm
length ws

Caso Inductivo

Izq)

length ((x:xs') ++ ws)
        -------------
=               def de ++
length (x : (xs' ++ ws))
------------------------
=               def length
1 + length (xs' ++ ws)
    ------------------
=               HI
1 + length xs' + length ws


Der)

length (x:xs') + length ws
--------------
=               def de length
1 + length xs' + length ws 

-- b

para todo xs. para todo ys.  para todo zs. (xs ++ ys) ++ zs = xs ++ (ys ++ zs)
Dem) sean ns, ms y os tres listas. Por ppio de induccion sobre la estructura de zs, es equivalente demostrar que:

Caso Base: ns = []

([] ++ ms) ++ os = [] ++ (ms ++ os) ?

Caso Inductivo: ns = x: xs'

HI) (xs' ++ ms) ++ os = xs' ++ (ms ++ os) !
TI) ((x:xs') ++ ms) ++ os = (x:xs') ++ (ms ++ os) ?

Caso Base:

I)

([] ++ ms) ++ os 
----------
=               def ++
ms ++ os

D)

[] ++ (ms ++ os)
----------------
=               def ++
ms ++ os

Caso Inductivo:

I)

((x:xs') ++ ms) ++ os
--------------
=               def ++
x : (xs' ++ ms) ++ os
    -----------------
=               HI
x : (xs' ++ (ms ++ os))


D)

(x:xs') ++ (ms ++ os)
---------------------
=               def ++
x : (xs' ++ (ms ++ os))

-- c

Prop)
count (const True) = length
por ppio de extensionalidad
para todo xs. count (const True) xs = length xs?
sea zs una lista finita, por ppio de induccion sobre la estructura de zs, es equivalente demostrar que:

Caso Base: zs = []

count (const True) [] = length []?

Caso Inductivo: zs = (x:xs')

HI) count (const True) xs' = length xs' !
TI) count (const True) (x:xs') = length (x:xs') ?


Caso Base:

I)

count (const True) []
---------------------
=                   def count
0

D)

length []
---------
=                   def length
0


Caso Inductivo:

I)

count (const True) (x:xs')
--------------------------
=                   def count
sumarSiCumple (const True) x + count (const True) xs'
----------------------------
=                   def sumarSiCumple
1 + count (const True) xs'
    ---------------------
=                   HI
1 + length xs'

D)

length (x:xs')
--------------
=                   def length
1 + length xs'

-- d

Prop)
elem =  any . (==)

por ppio de extensionalidad
para todo y. elem y = (any . (==)) y
por def de (.)
para todo y. elem y = any ((==) y)
para todo y. para todo ys = elem y ys = any ((==) y) ys
sea z un elem cualquiera y zs una lista finita. Por ppio de induccion sobre la estructura de zs, es equivalente demostrar que:

Caso Base: zs = []

elem y [] = any ((==) y) [] ?

Caso Inductivo: zs = x:xs)

HI) elem y xs = any ((==) y) xs !
TI) elem y (x:xs) = any ((==) y) (x:xs) ?

Caso Base:

I)

elem y [] 
---------
=           def elem
False

D)

any ((==) y) []
---------------
=           def any
False

Caso Inductivo:

I)

elem y (x:xs)
-------------
=           def elem
y == x || elem y xs

D)

any ((==) y) (x:xs)
-------------------
=           def any
(==) y x || any ((==) y) xs
            ---------------
=           HI
(==) y x || elem y xs

-- e

Prop)

para todo x. any (elem x) = elem x . concat
para todo x. para todo xss. any (elem x) xss = (elem x . concat) xss ?
por def de (.)
para todo x. para todo xss. any (elem x) xss = elem x (concat xss) ?
sea y un elemento, yss una lista de listas. Por ppio de induccion sobre la estructura de yss, es equivalente demostrar que:

Caso Base: yss = []

any (elem x) [] = elem x (concat [])?

Caso Inductivo: yss = (ys:yss')

HI) any (elem x) yss' = elem x (concat yss') !
TI) any (elem x) (ys:yss') = elem x (concat (ys:yss')) ?

Caso Base:

I)

any (elem x) []
---------------
=               def any
False

D)

elem x (concat [])
        ----------
=               def concat
elem x []
---------
=               def elem
False

Caso Inductivo:

I)

any (elem x) (ys:yss')
---------------------
=               def any
(elem x) ys || any (elem x) yss'
                ---------------
=               HI
(elem x) ys || elem x (concat yss')
-----------------------------------
=               lema elemDistributivo
elem x (ys ++ concat yss')

D)

elem x (concat (ys:yss'))
        --------------
=               def concat
elem x (ys ++ concat yss')

lema elemDistributivo:

#NOTA
Como mierda me doy cuenta de esto?
mira la parte de (elem x) ys || elem x (concat yss') del lado izquierdo, fijate que
del lado derecho te queda esto elem x (ys ++ concat yss'), tienen en comun que cosa?
la (concat yss'), es una lista, otra cosa que hay que fijarse es que esta pasando con el
(elem x), fijate que esta siendo como distribuido la parte de la D es consecuencia de la I
al distribuir eso. Para sacar el lema tenes que generalizar( o sea, fijarte las partes en comun y ponerle una lista )


por ppio de extensionalidad
para todo x. para todo xs. para todo ys. (elem x) xs || elem x ys = elem x (xs ++ ys)
sean x' un elemento cualquiera, ss y ns dos listas cualquiera. Por ppio de induccion sobre la estructura de ss, es equivalente demostrar que:

Caso Base: ss = []

(elem x') [] || elem x' ns = elem x' ([] ++ ns) ?

Caso Inductivo: ss = (s: ss')

HI) (elem x') ss' || elem x' ns = elem x' (ss' ++ ns) !
TI) (elem x') (s:ss') || elem x' ns = elem x' ((s:ss') ++ ns) ?

Caso Base:

I)

(elem x') [] || elem x' ns
------------
=                   def elem
False || elem x' ns
-------------------
=                   neutro ||
elem x' ns

D)

elem x' ([] ++ ns)
        ---------
=                   def de ++
elem x' ns

Caso Inductivo:

I)

(elem x') (s:ss') || elem x' ns
-----------------
=                   def de elem
x' == s || elem x' ss' || elem x' ns


D)

elem x' ((s:ss') ++ ns)
        ---------------
=                   def de ++
elem x' (s : (ss' ++ ns))
-------------------------
=                   def de elem
x' == s || elem x' (ss' ++ ns)
            ------------------
=                   HI
x' == s || elem x' ss' || elem x' ns

-- f

para todo xs. para todo ys. subset xs ys = all (flip elem ys) xs
sean xn, yn dos listas finitas. por ppio de induccion sobre xs, es equivalente demostrar que:

Caso Base: xn = []

subset [] yn = all (flip elem yn) [] ?

Caso Inductivo: xn = x:xs')

HI) subset xs' yn = all (flip elem yn) xs' !
TI) subset (x:xs') yn = all (flip elem yn) (x:xs') ?

Caso Base:

I)

subset [] yn 
------------
=               def subset
True

D)

all (flip elem yn) []
---------------------
=               def all
True

Caso Inductivo:

I)

subset (x:xs') yn
-----------------
=               def subset
elem x yn && subset xs' yn

D)

all (flip elem yn) (x:xs')
--------------------------
=               def all
(flip elem yn) x && all (flip elem yn) xs'
----------------
=               def flip
elem x yn && all (flip elem yn) xs'
             ----------------------
=               HI
elem x yn && subset xs' yn

-- g

Prop) all null = null . concat?
por ppio de extensionalidad.
para todo xss. all null xss = (null . concat) xss?
por def de (.)
para todo xss. all null xss = null (concat xss)?
sea nss una lista de lista. por ppio de induccion sobre la estructura de nss, es equivalente demostrar que:

Caso Base: nss = []

all null [] = null (concat []) ?

Caso Inductivo: nss = xs:xss')

HI) all null xss' = null (concat xss') !
TI) all null (xs:xss') = null (concat (xs:xss)) ?

Caso Base:

I)

all null []
-----------
=               def all
True

D)

null (concat [])
      ---------
=               def concat
null []
-------
=               def null
True

Caso Inductivo:

I)

all null (xs:xss')
------------------
=               def de all
null xs && all null xss'
           ------------
=               HI
null xs && null (concat xss')
-----------------------------
=               Lema
null (xs ++ concat xss)

D)

null (concat (xs:xss))
      ---------------
=               def de concat
null (xs ++ concat xss)

-- h)

Prop) 
length = length . reverse
por ppio de extensionalidad
para todo xs. length xs = (length . reverse) xs?
por def de (.)
para todo xs. length xs = length (reverse xs)?
sea ns una lista. por ppio de induccion sobre la estructura de ns, es equivalente demostrar que:

Caso Base: ns = []

length [] = length (reverse [])?

Caso Inductivo: ns = (x:xs')

HI) length xs' = length (reverse xs') !
TI) length (x:xs') = length (reverse (x:xs')) ?

Caso Base:

I)

length []

D)

length (reverse [])
        ----------
=               def reverse
length []

Caso Inductivo:

I)

length (x:xs')
--------------
=               def de length
1 + length xs'
    ---------
=               HI
1 + length (reverse xs')

D)

length (reverse (x:xs'))
        ---------------
=               def reverse
length (reverse xs' ++ [x])
---------------------------
=               demo 1
length (reverse xs') + length [x]
                       ----------
=               def length
length (reverse xs') + 1 + length []
                            --------
=               def length
length (reverse xs') + 1 + 0
----------------------------
=               aritm
1 + length (reverse xs')

-- i
para todo xs. para todo ys. reverse (xs ++ ys) = reverse ys ++ reverse xs?
sea ns y ss dos listas, por ppio de induccion sobre la estructura de ns, es equivalente demostrar que:

Caso Base: ns = [])

reverse ([] ++ ss) = reverse ss ++ reverse [] ?

Caso Inductivo: ns = x:xs')

HI) reverse (xs' ++ ss) = reverse ss ++ reverse xs' !
TI) reverse ((x:xs') ++ ss) = reverse ss ++ reverse (x:xs') ?

Caso Base:

I)

reverse ([] ++ ss)
        ---------
=               def ++
reverse ss

D)

reverse ss ++ reverse []
              ----------
=               def reverse
reverse ss ++ []
----------------
=               def de ++
reverse ss


Caso Inductivo:

I)

reverse ((x:xs') ++ ss)
        --------------
=               def de ++
reverse (x : (xs' ++ ss))
-------------------------
=               def de reverse
reverse (xs' ++ ss) ++ [x]
-------------------
=               HI
reverse ss ++ reverse xs' ++ [x]


D)

reverse ss ++ reverse (x:xs')
              ---------------
=               def reverse
reverse ss ++ reverse xs' ++ [x]


-- j

Prop)
para todo xs. para todo ys. all p (xs ++ ys) = all p (reverse xs) && all p (reverse ys) ?
sea ns y ss, dos listas. Por ppio de induccion sobre la estructura de ns, es equivalente demostrar que:

CASO BASE: ns = []

all p ([] ++ ss) = all p (reverse []) && all p (reverse ss) ?

CASO Inductivo: ns = (x:xs')

HI) all p (xs' ++ ss) = all p (reverse xs') && all p (reverse ss) !
TI) all p ((x:xs') ++ ss) = all p (reverse (x:xs')) && all p (reverse ss) ?


CASO BASE:

I)

all p ([] ++ ss)
        -------
=               def de ++
all p ss
--------
=               lema equisDe
all p (reverse ss)


D)

all p (reverse []) && all p (reverse ss)
        ---------
=               def de reverse
all p [] && all p (reverse ss)
--------
=               def de all
True && all p (reverse ss)
--------------------------
=               True es el neutro en &&
all p (reverse ss)


def de lema equisDe
all p = all p . reverse
por ppio de extensionalidad
para todo xs. all p xs = (all p . reverse) xs
por def de (.)
para tdo xs. all p xs = all p (reverse xs) ?
sea ns una lista cualquiera, por ppio de induccion sobre la estructura de ns, es equivalente demostrar que:

Caso Base: ns = []

all p [] = all p (reverse []) ?

Caso Inductivo: ns = (x:xs')

HI) all p xs' = all p (reverse xs') !
TI) all p (x:xs') = all p (reverse (x:xs')) ?


Caso Base:

I)

all p []

D)

all p (reverse [])
        ---------
=               def de reverse
all p []


Caso Inductivo:

I)

all p (x:xs')
-------------
=               def all
p x && all p xs'
       --------
=               HI
p x && all p (reverse xs')


D)

all p (reverse (x:xs'))
        --------------
=               def de reverse
all p (reverse xs' ++ [x])
--------------------------
=               lema 2: distributivo
all p (reverse xs') && all p [x]
                        --------
=               def de all
all p (reverse xs') && p x && all p []
                                  --------
=               def de all
all p (reverse xs') && p x && True
                        ---------------
=               True es el neutro de &&
all p (reverse xs') && p x
--------------------------
=               conmutatividad &&
p x && all p (reverse xs')


definicion de lema:
por ppio de extensionalidad (dos veces)
para todo xs. para todo ys. all p (xs ++ ys) = all p xs && all p ys ?
sea ns y ss, dos listas cualquiera. por ppio de induccion sobre la estructura de ns, es equivalente demostrar que:

Caso Base: ns = []

all p ([] ++ ss) = all p [] && all p ss ?

Caso Inductivo: ns = (x:xs')

HI) all p (xs' ++ ss) = all p xs' && all p ss !
TI) all p ((x:xs') ++ ss) = all p (x:xs') && all p ss ?


Caso Base:

I)

all p ([] ++ ss)
        -------
=               def de ++
all p ss

D)

all p [] && all p ss
--------
=               def de all
True && all p ss
----------------
=               True es neutro de &&
all p ss

Caso Inductivo:

I)

all p ((x:xs') ++ ss)
        ------------
=               def de ++
all p (x : (xs' ++ ss))
-----------------------
=               def de all
p x && all p (xs' ++ ss)

D)

all p (x:xs') && all p ss
-------------
=               def de all
p x && all p xs' && all p ss
        --------------------
=               HI
p x && all p (xs' ++ ss)

------------------------------------------------------------------------------------

-- i.
para todo n1. para todo n2. evalN (addN n1 n2) = evalN n1 + evalN n2 ?
sea m1 y m2 dos elementos de tipo N, por ppio de induccion en la estructura de m1, es equivalente demostrar que:

Caso Base: m1 = Z

evalN (addN Z m2) = evalN Z + evalN m2

Caso Inductivo: m1 = S n)

HI) evalN (addN n m2) = evalN n + evalN m2 !
TI) evalN (addN (S n) m2) = evalN (S n) + evalN m2 ?

Caso Base:

I)

evalN (addN Z m2)
        --------
=               def de addN
evalN m2

D)

evalN Z + evalN m2
-------
=               def de evalN
0 + evalN m2
------------
=               nuetro de la suma
evalN m2

Caso Inductivo:

I)

evalN (addN (S n) m2)
        ------------
=               def de addN
evalN (S (addN n m2))
---------------------
=               def de evalN
1 + evalN (addN n m2)
    -----------------
=               def evalN
1 + evalN n + evalN m2

D)

evalN (S n) + evalN m2
-----------
=               def de evalN
1 + evalN n + evalN m2

-- ii.  

para todo n1. para todo n2. evalN (prodN n1 n2) = evalN n1 * evalN n2 
sea m1 y m2 dos elementos de tipo N. Por ppio de induccion sobre m1, es equivalente demostrar que:

Caso Base: m1 = Z

evalN (prodN Z m2) = evalN Z * evalN m2 ?

Caso Inductivo: m1 = S n)

HI) evalN (prodN n m2) = evalN n * evalN m2 !
TI) evalN (prodN (S n) m2) = evalN (S n) * evalN m2 ?

Caso Base:

I)

evalN (prodN Z m2)
        ---------
=               def prodN
evalN Z
-------
=               def evalN
0

D)

evalN Z * evalN m2
-------
=               def de evalN
0 * evalN m2
------------
=               def de mult
0

Caso Inductivo:

I)

evalN (prodN (S n) m2)
        -------------
=               def prodN
evalN (addN (prodN n m2) m2)
---------------------------
=               prop demostrada ejercicio anterior
evalN (prodN n m2) + evalN m2


D)

evalN (S n) * evalN m2
-----------
=               def de evalN
(1 + evalN n) * evalN m2
------------------------
=               distributiva
evalN m2 + evalN n * evalN m2
          -------------------
=               HI
evalN m2 + evalN (prodN n m2)


-- iii.

int2N . evalN = id
Dem) por ppio de extensionalidad.
para todo m. (int2N . evalN) m = id m?
por def de (.)
para todo m. int2N (evalN m) = id m?
sea n un elemento de tipo N. Por ppio de induccion sobre la estructura de n, es equivalente demostrar que:

Caso Base: m = Z

int2N (evalN Z) = id Z ?

Caso Inductivo: m = (S m')

HI) int2N (evalN m') = id m' !
TI) int2N (evalN (S m')) = id (S m') !


Caso Base:

I)

int2N (evalN Z)
       --------
=               def evalN
int2N 0
-------
=               def int2N
Z

D)

id Z
----
=               def id
Z

Caso Inductivo:

I)

int2N (evalN (S m'))
        -----------
=               def evalN
int2N (1 + evalN m')
-------------------
=               def int2N
S (int2N (1 + evalN m') - 1 )
          ------------------
=               cancelo los 1
S (int2N(evalN m'))
-------------------
=               HI
id (S m')

D)

id (S m')

-- iv.
-- Dem) evalN . int2N = id ?
-- Prop) por ppio de extensionalidad.
-- para todo n. (evalN . int2N) n = id n ?
-- por def de (.)
-- para todo n. evalN (int2N n) = id n ?
-- sea n' un numero, por ppio de induccion en la estructura de n, es equivalente demostrar que:

-- Caso Base: n' = 0

-- evalN (int2N 0) = id 0 ?

-- Caso Inductivo: n' = m)

-- HI) evalN (int2N m) = id m !
-- TI) evalN (int2N n) = id n

Preguntarle a santi

-- Ejercicio 2) b

i.  evalNU . succNU = (+1) . evalNU 
por ppio de extensionalidad.
para todo nu. (evalNU . succNU) nu = ((+1) . evalNU) nu ?
por def de (.)
evalNU (succNU nu) = (+1) (evalNU nu) ?
sea nu' un tipo NU, por ppio de induccion sobre la estructura de nu', es equivalente demostrar que:

CASO BASE: nu' = []

evalNU (succNU []) = (+1) (evalNU []) ?

Caso Inductivo: nu' = n:ns)

HI) evalNU (succNU ns) = (+1) (evalNU ns) ?
TI) evalNU (succNU (n:ns)) = (+1) (evalNU (n:ns)) ?

Caso Base

I)

evalNU (succNU [])
        ---------
=               def de succNU
evalNU (() : [])
----------------
=               def de evalNU
1 + evalNU []
    ---------
=               def evalNU
1 + 0

D)

(+1) (evalNU [])
     -----------
=               def evalNU
(+1) 0

Caso Inductivo

I)

evalNU (succNU (n:ns))
        -------------
=               def de succNU
evalNU ((): (n:ns))
-------------------
=               def de evalNU
1 + evalNU (n:ns)
    --------------
=               HI
1 + 1 + evalNU ns


D)

(+1) (evalNU (n:ns))
      -------------
=               def evalNU
(+1) (1 + evalNU ns)


ii. para todo n1. para todo n2. evalNU (addNU n1 n2) = evalNU n1 + evalNU n2 
sean nu1 y nu2, dos elementos de tipo NU. Por ppio de induccion en nu1, es equivalente demostrar que:

Caso Base: nu1 = []

evalNU (addNU [] nu2) = evalNU [] + evalNU nu2 ?

Caso Inductivo: nu1 = n:ns)

HI) evalNU (addNU ns n2) = evalNU ns + evalNU n2 !
TI) evalNU (addNU (n:ns) n2) = evalNU (n:ns) + evalNU n2 ?

Caso Base:

I)

evalNU (addNU [] nu2)
        ------------
=               def addNU
evalNU nu2

D)

evalNU [] + evalNU nu2
---------
=               def de evalNU
0 + evalNU nu2
--------------
=               0 es neutro
evalNU nu2

Caso Inductivo

I)

evalNU (addNU (n:ns) n2)
        ---------------
=               def de addNU
evalNU (n : addNU ns n2)
------------------------
=               def evalNU
1 + evalNU (addNU ns n2)
    --------------------
=               HI
1 + evalNU ns + evalNU n2


D)

evalNU (n:ns) + evalNU n2
-------------
=               def de evalNU
1 + evalNU ns + evalNU n2


iv.  n2nu . nu2n = id 
por ppio de extensionalidad.
para todo n. (n2nu . nu2n) n = id n ?
por def de (.)
para todo n. n2nu (nu2n n) = id n ?
sea nu de tipo NU. por ppio de induccion en la estructura de nu, es equivalente:

Caso Base: nu = []

n2nu (nu2n []) = id [] ?

Caso Inductivo: nu = (n:nu')

HI) n2nu (nu2n nu') = id nu' !
TI) n2nu (nu2n (n:nu')) = id (n:nu') ?


Caso Base

I)

n2nu (nu2n [])
     ---------
=               def de nu2n
n2nu Z
------
=               def de n2nu
[]


D)

id []
=               def de id
[]

Caso Inductivo

I)

n2nu (nu2n (n:nu'))
      ------------
=               def de nu2n
n2nu (S (nu2n nu'))
-------------------
=               def n2nu
() : (n2nu (nu2n nu'))
     ----------------
=               HI
() : id nu'
     -----
=               def de id
() : nu'

D)

id (n:nu')
----------
=               HI
(n:nu')


-- ejercicio 3)b

i.  evalNB . normalizarNB = evalNB 
Prop) por ppio de extensionalidad
para todo nbin. (evalNB . normalizarNB) nbin = evalNB nbin ?
por def de (.)
para todo nbin. evalNB (normalizarNB nbin) = evalNB nbin ?
sea nbin1 de tipo NBin, por ppio de induccion sobre la estructura de nbin1, es equivalente demostrar:

CASO BASE: nbin = []

evalNB (normalizarNB []) = evalNB [] ?

CASO INDUCTIVO: nbin = (n:ns)

SUBCASO n = O 

HI) evalNB (normalizarNB ns) = evalNB ns !
TI) evalNB (normalizarNB (O:ns)) = evalNB (O:ns) ?

SUBCASO n = I

HI) evalNB (normalizarNB ns) = evalNB ns !
TI) evalNB (normalizarNB (I:ns)) = evalNB (I:ns) ?

Caso Base:

I)

evalNB (normalizarNB [])
        ---------------
=                       def de normalizarNB
evalNB []

Caso Inductivo: SUBCASO n = O

I)

evalNB (normalizarNB (O:ns))
        ---------------------
=                       def de normalizarNB
evalNB (normalizar O (normalizarNB ns))
---------------------------------------
=                       Lema PorDosParaLlegar                     
2 * evalNB (normalizarNB ns)

D)

evalNB (O:ns)
---------------
=                       def evalNB
dbAsInt O + 2 * ( evalNB ns )
                -------------
=                       HI
dbAsInt O + 2 * evalNB (normalizarNB ns)
---------
=                       def dbAsInt
0 + 2 * evalNB (normalizarNB ns)
--------------------------------
=                       cero neutro suma
2 * evalNB (normalizarNB ns)

Caso Inductivo: SUBCASO n = I

I)

evalNB (normalizarNB (I:ns))
        -------------------
=                       def normalizarNB
evalNB (normalizar I (normalizarNB ns))
        ------------------------------
=                       def de normalizar
evalNB (I : (normalizarNB ns))
------------------------------
=                       def de evalNB
dbAsInt I + 2 * (evalNB (normalizarNB ns))
                -------------------------
=                       HI
dbAsInt I + 2 * evalNB ns


D)

evalNB (I:ns)
-------------
=                       def evalNB
dbAsInt I + 2 * (evalNB ns)



Def de Lema PorDosParaLlegar
Prop) por ppio de extensionalidad
para todo nb. evalNB (normalizar O nb) = 2 * (evalNB nb) ?
sea xs una lista. Por ppio de induccion sobre la estructura de xs, se vera que:

Caso Base: xs = []

evalNB (normalizar O []) = 2 * (evalNB []) ?

Caso Inductivo: xs = (n:nb')

HI) evalNB (normalizar O nb') = 2 * (evalNB nb') !
TI) evalNB (normalizar O (n:nb')) = 2 * (evalNB (n:nb')) ?

Caso Base:

I)

evalNB (normalizar O [])
        ---------------
=                       def de normalizar
evalNB []
---------
=                       def de evalNB
0

D)

2 * (evalNB [])
     ---------
=                       def de evalNB
2 * 0
-----
=                       def de *
0

Caso Inductivo:

I)

evalNB (normalizar O (n:nb'))
        --------------------
=                       def de normalizar
evalNB (O : (n:nb'))
--------------------
=                       def de evalNB
dbAsInt O + 2 * (evalNB (n:nb'))
---------
=                       def de dbAsInt
0 + 2 * (evalNB (n:nb'))
------------------------
=                       neutro de suma
2 * (evalNB (n:nb'))

D)

2 * (evalNB (n:nb'))


-- ii.  evalNB . succNB = (+1) . evalNB 
Prop) por ppio de extensionalidad
para todo nbin. (evalNB . succNB) nbin = ((+1) . evalNB) nbin ?
por def de (.) 2 veces
para todo nbin. evaln (succNB nbin) = (+1) (evalNB nbin) ?
sea nbin1 de tipo NBin, por ppio de induccion sobre la estructura de nbin1, es equivalente demostrar:

CASO BASE: nbin = []

evalNB (succNB []) = (+1) (evalNB []) ?

CASO INDUCTIVO: nbin = n: ns)

Subcaso nbin = O

HI) evalNB (succNB ns) = (+1) (evalNB ns) !
TI) evalNB (succNB (O: ns)) = (+1) (evalNB (O: ns)) ?

Subcaso nbin = I

HI) evalNB (succNB ns) = (+1) (evalNB ns) !
TI) evalNB (succNB (I: ns)) = (+1) (evalNB (I: ns)) ?

Caso Base

I)

evalNB (succNB [])
        ---------
=               def de succNB
evalNB [I]
---------
=               def de evalNB
dbAsInt I + 2 * (evalNB [])
---------
=               def de dbAsInt
1 + 2 * (evalNB [])
        -----------
=               def de evalNB
1 + 2 * 0
   -------
=               def de *
1

D)

(+1) (evalNB [])
     -----------
=               def de evalNB
(+1) 0
------
=               def de +
1

Caso Inductivo

I)

evalNB (succNB (O: ns))
        ---------------------
=               def de succNB
evalNB ( I : ns)
----------------
=               def de evalNB
dbAsInt I + 2 * (evalNB ns)
---------
=               def de dbAsInt
1 + 2 * (evalNB ns)


D)

(+1) (evalNB (O: ns))
      --------------
=               def de evalNB
(+1) (dbAsInt O + 2 * (evalNB ns))
     ---------
=               def de dbAsInt
(+1) (0 + 2 * (evalNB ns))
      -------------------
=               def de +
1 + 2 * (evalNB ns)

Caso Inductivo: n = I

I)

evalNB (succNB (I: ns))
        --------------
=               def de succNB
evalNB (O : (succNB ns))
----------------------
=               def de evalNB
dbAsInt O + 2 * ( evalNB (succNB ns) )
---------
=               def de dbAsInt
0 + 2 * ( evalNB (succNB ns))
----------------------------
=               def de +
2 * ( evalNB (succNB ns))
     -------------------
=               HI
2 * (+1) (evalNB ns)

D)

(+1) (evalNB (I: ns))
      --------------
=               def de evalNB
(+1) (dbAsInt I + 2 * (evalNB ns))
      ---------
=               def de dbAsInt
(+1) 1 + 2 * (evalNB ns)
---------
=               def de +
2 + 2 * (evalNB ns)
-------------------
=               factor comun
2 * (+1) (evalNB ns)

-- iii.
para todo n1. para todo n2. evalNB (addNB n1 n2) = evalNB n1 + evalNB n2 ?
sea nb1 y nb2 de tipo NBin, por ppio de induccion en la estructura de nb1, se vera que:

CASO BASE: nb1 = []

evalNB (addNB [] n2) = evalNB [] + evalNB n2 ?

Subcaso nbin = O:n1

HI) evalNB (addNB n1 n2) = evalNB n1 + evalNB n2 !
TI) evalNB (addNB (O:n1) n2) = evalNB (O:n1) + evalNB n2 ?

Subcaso nbin = I:n1

HI) evalNB (addNB n1 n2) = evalNB n1 + evalNB n2 !
TI) evalNB (addNB (I:n1) n2) = evalNB (I:n1) + evalNB n2 ?

Caso Base:

I)

evalNB (addNB [] n2)
        -----------
=               def de addNB
evalNB n2

D)

evalNB [] + evalNB n2
---------
=               def de evalNB
0 + evalNB n2
-------------
=               def de +
evalNB n2

Caso Inductivo:

I)

evalNB (addNB (O:n1) n2)
        ---------------
=               def de addNB
evalNB (f I  )

D)

evalNB (O:n1) + evalNB n2



iv. prop) nb2n . n2nb = id 
dem) por ppio de extensionalidad
para todo n. (nb2n . n2nb) n = id n ?
por def de (.)
para todo n. nb2n (n2nb n) = id n ?
sea m un elemento de tipo N, por ppio de induccion sobre la estructura de m, se vera

Caso Base: m = Z

nb2n (n2nb Z) = id Z ?

Caso Inductivo: m = (S n')

HI) nb2n (n2nb n') = id n' ! 
TI) nb2n (n2nb (S n')) = id (S n') ? 


Caso Base:

I)

nb2n (n2nb Z)
     --------
=               def de n2nb
nb2n []
-------
=               def de nb2n
Z

D)

id Z
----
=               def de id
Z

Caso Inductivo:

I)

nb2n (n2nb (S n'))
      -----------
=               def de n2nb
nb2n (succNB (n2nb n'))
     ------------------
=               


D)

id (S n')
---------
=               HI
nb2n (n2nb n')

-- v.  normalizarNB . normalizarNB = normalizarNB 

-- c.  solamente  una  de  las  siguientes  propiedades  es  verdadera.  Dar  un 
-- contraejemplo para la que no lo sea, y demostrar la que sí lo sea. 

-- i.  n2nb . nb2n = id 
-- ii.  n2nb . nb2n = normalizarNB 


-- Demostraciones ejercicio 1 seccion III

i.  evalExpA . simplificarExpA = evalExpA 
Dem) por ppio de extensionalidad 
para todo ea. (evalExpA . simplificarExpA) ea = evalExpA ea ?
por def de (.)
para todo ea. evalExpA (simplificarExpA ea) = evalExpA ea?
sea expa de tipo ExpA, por ppio de induccion sobre la estructura de expa, se vera que:

Caso Base: expa = Cte n)

evalExpA (simplificarExpA (Cte n)) = evalExpA (Cte n) ?

Caso Inductivo 1: expa = Suma expa1 expa2)

HI1) evalExpA (simplificarExpA expa1) = evalExpA expa1 !
HI2) evalExpA (simplificarExpA expa2) = evalExpA expa2 !
TI) evalExpA (simplificarExpA (Suma expa1 expa2)) = evalExpA (Suma expa1 expa2) ?

Caso Inductivo 2: expa = Prod expa1 expa2)

HI1) evalExpA (simplificarExpA expa1) = evalExpA expa1 !
HI2) evalExpA (simplificarExpA expa2) = evalExpA expa2 !
TI) evalExpA (simplificarExpA (Prod expa1 expa2)) = evalExpA (Prod expa1 expa2) ?

Caso Base:

I)

evalExpA (simplificarExpA (Cte n))
         -------------------------
=               def de simplificarExpA
evalExpA (Cte n)

D)

evalExpA (Cte n)


Caso Inductivo 1:

I)

evalExpA (simplificarExpA (Suma expa1 expa2))
          ----------------------------------
=               def de simplificarExpA
evalExpA (simplificarSuma (simplificarExpA expa1) (simplificarExpA expa2))
--------------------------------------------------------------------------
=               Lema evalExpA-distribuido

D)

evalExpA (Suma expa1 expa2)
---------------------------
=               def de evalExpA
evalExpA e1 + evalExpA e2
-----------
=               HI1
evalExpA (simplificarExpA expa1) + evalExpA e2
                                   -----------
=               HI2
evalExpA (simplificarExpA expa1) + evalExpA (simplificarExpA expa2)


Caso Inductivo 2:

I)

evalExpA (simplificarExpA (Prod expa1 expa2))
          ----------------------------------
=               def de simplificarExpA
evalExpA (simplificarProd (simplificarExpA expa1) (simplificarExpA expa2))
--------------------------------------------------------------------------
=               Lema evalExpA-distribuido-Suma
evalExpA (simplificarExpA expa1) * evalExpA (simplificarExpA expa2)


D)

evalExpA (Prod expa1 expa2)
---------------------------
=               def de evalExpA
evalExpA e1 * evalExpA e2
-----------
=               HI1
evalExpA (simplificarExpA expa1) * evalExpA e2
                                   -----------
=               HI2
evalExpA (simplificarExpA expa1) * evalExpA (simplificarExpA expa2)



Definicion de Lema, evalExpA-distribuido
por ppio de extensionalidad (dos veces)
para todo e1. para todo e2. evalExpA (simplificarSuma e1 e2) = evalExpA e1 + evalExpa e2 ?

voy a demostrar por casos:

Caso e1 = Cte 0)

evalExpA (simplificarSuma (Cte 0) e2) = evalExpA (Cte 0) + evalExpa e2 ?

I)

evalExpA (simplificarSuma (Cte 0) e2)
          ---------------------------
=               def de simplificarSuma
evalExpA e2

D)

evalExpA (Cte 0) + evalExpa e2
----------------
=               def de evalExpA
0 + evalExpA e2
---------------
=               cero es el neutro
evalExpa e2


Caso e2 = Cte 0)

evalExpA (simplificarSuma e1 (Cte 0)) = evalExpA e1 + evalExpA (Cte 0) ?

I)

evalExpA (simplificarSuma e1 (Cte 0))
         ----------------------------
=               def de simplificarSuma
evalExpA e1

D)

evalExpA e1 + evalExpA (Cte 0)
              ----------------
=               def de evalExpA
evalExpA e1 + 0
---------------
=               cero es el neutro
evalExpa e1


Caso e1 y e2 != Cte 0)

evalExpA (simplificarSuma e1 e2) = evalExpA e1 + evalExpA e2 ?

I)

evalExpA (simplificarSuma e1 e2)
          ---------------------
=               def de simplificarSuma
evalExpA (Suma e1 e2)
---------------------
=               def de evalExpA
evalExpA e1 + evalExpA e2


D)

evalExpA e1 + evalExpA e2

Lema evalExpA-distribuido-Suma

Prop ) por ppio de extensionalidad
para todo expa1. para todo expa2. 
evalExpA (simplificarProd (simplificarExpA expa1) (simplificarExpA expa2)) = evalExpA (simplificarExpA expa1) * evalExpA (simplificarExpA expa2) ?
-- CASO1 e1 = (Cte 0)

-- evalExpA (simplificarProd (Cte 0) e2) = evalExpA (Cte 0) * evalExpA e2 ?

-- LI)

-- evalExpA (simplificarProd (Cte 0) e2)
--          ---------------------------
-- =                                       def evalExpA
-- evalExpA (Cte 0)
-- ----------------
-- =                                       def evalExpA
-- 0 

-- LD)

-- evalExpA (Cte 0) * evalExpA e2
-- ----------------
-- =                                       def evalExpA
-- 0 * evalExpA e2
-- ---------------
-- =                                       cero es absorbente
-- 0

-- CASO2: e2 = (Cte 0)

-- evalExpA (simplificarProd e1 (Cte 0)) = evalExpA e1 * evalExpA (Cte 0) ?

-- LI)

-- evalExpA (simplificarProd e1 (Cte 0))
--          ---------------------------
-- =                                       def simplificarProd
-- evalExpA (Cte 0)

-- LD)

-- evalExpA e1 * evalExpA (Cte 0)
--               ----------------
-- =                                       def evalExpA
-- evalExpA e1 * 0
-- ---------------
-- =                                       cero es absorbente
-- 0




-- CASO3: e1 = (Cte 1)

-- evalExpA (simplificarProd (Cte 1) e2) = evalExpA (Cte 1) * evalExpA e2 ?

-- LI)

-- evalExpA (simplificarProd (Cte 1) e2)
--          ----------------------
-- =                                       def simplificarProd
-- evalExpA e2


-- LD)

-- evalExpA (Cte 1) * evalExpA e2
-- ----------------
-- =                                       def evalExpA
-- 1 * evalExpA e2
-- ---------------
-- =                                       1 es neutro en la multiplicacion
-- evalExpA e2

-- CASO4: e2 = (Cte 1)

-- evalExpA (simplificarProd e1 (Cte 1)) = evalExpA e1 * evalExpA (Cte 1) ?

-- LI)

-- evalExpA (simplificarProd e1 (Cte 1))
--          ---------------------------
-- =                                       def simplificarProd
-- evalExpA e1                             


-- LD)

-- evalExpA e1 * evalExpA (Cte 1)
--               ----------------
-- =                                       def evalExpA
-- evalExpA e1 * 1
-- ---------------
-- =                                       1 es neutro en la multiplicacion
-- evalExpA e1

-- CASO5: e1 && e2 != (Cte 1) && (Cte 0)

-- evalExpA (simplificarProd e1 e2) = evalExpA e1 * evalExpA e2 ?

-- LI)

-- evalExpA (simplificarProd e1 e2)
--          ---------------------------
-- =                                       def simplificarProd
-- evalExpA (Prod e1 e2)
-- ---------------------
-- =                                       def evalExpA
-- evalExpA e1 * evalExpA e2                         


-- LD)

-- evalExpA e1 * evalExpA e2

ii.  cantidadSumaCero . simplificarExpA = const 0
por ppio de extensionalidad 
para todo expa. (cantidadSumaCero . simplificarExpA) expa= const 0 expa ?
por def de (.)
para todo expa. cantidadSumaCero (simplificarExpA expa) = const 0 expa ?
por def de const
para todo expa. cantidadSumaCero (simplificarExpA expa) = 0 ?
sea expa1 de tipo ExpA, por ppio de induccion sobre la estructura de expa1, es equivalente demostrar que:

Caso Base: expa = (Cte n)

cantidadSumaCero (simplificarExpA (Cte n)) = 0 ?

Caso Inductivo 1: expa = (Suma e1 e2)
 
HI1) cantidadSumaCero (simplificarExpA e1) = 0 !
HI2) cantidadSumaCero (simplificarExpA e2) = 0 !
TI) cantidadSumaCero (simplificarExpA (Suma e1 e2)) = 0 ?

Caso Inductivo 2: expa = (Prod e1 e2)
 
HI1) cantidadSumaCero (simplificarExpA e1) = 0 !
HI2) cantidadSumaCero (simplificarExpA e2) = 0 !
TI) cantidadSumaCero (simplificarExpA (Prod e1 e2)) = 0 ?

Caso Base

I)

cantidadSumaCero (simplificarExpA (Cte n))
                  -----------------------
=               def de simplificarExpA
cantidadSumaCero (Cte n)
-----------------------
=               def de cantidadDeSumaCero
0

Caso Inductivo 1:

I)

cantidadSumaCero (simplificarExpA (Suma e1 e2))
                  ----------------------------
=               def de simplificarExpA
cantidadDeSumaCero (simplificarSuma (simplificarExpA expa1) (simplificarExpA expa2))
------------------------------------------------------------------------------------
=               Lema-distr-sumacero
cantidadSumaCero (simplificarExpA e1) + cantidadSumaCero (simplificarExpA e2)
-------------------------------------
=               HI1
0 + cantidadSumaCero (simplificarExpA e2)
    ------------------------------------
=               HI2
0 + 0

Caso Inductivo 2:

I)

cantidadSumaCero (simplificarExpA (Prod e1 e2))
                  -----------------------------
=               def de simplificarExpA
cantidadDeSumaCero (simplificarProd (simplificarProd e1) (simplificarProd e2))
=               def de lema distr-prod



HI1) cantidadSumaCero (simplificarExpA e1) = 0 !
HI2) cantidadSumaCero (simplificarExpA e2) = 0 !

-- ### LEMA llegarAHipotesis ###
-- por ppio de extensionalidad
-- para todo e1. para todo e2. cantidadDeSumaCero (simplificarProd e1 e2) = cantidadDeSumaCero e1 * cantidadDeSumaCero e2 ?
-- voy a demostrar por casos:

-- Caso1 = e1 = (Cte 0) y e2 es cualquier otra expresion

-- cantidadDeSumaCero (simplificarProd (Cte 0) e2) = cantidadDeSumaCero (Cte 0) * cantidadDeSumaCero e2 ?

-- LI)

-- cantidadDeSumaCero (simplificarProd (Cte 0) e2)
--                    ---------------------------
-- =                                       def simplificarProd
-- cantidadDeSumaCero (Cte 0)
-- -------------------------
-- =                                       def cantidadDeSumaCero
-- 0

-- LD)

-- cantidadDeSumaCero (Cte 0) * cantidadDeSumaCero e2
-- --------------------------
-- =                                       def cantidadDeSumaCero
-- 0 * cantidadDeSumaCero e2
-- -------------------------
-- =                                       0 absorbente
-- 0

-- Caso2 = e1 es cualquier otra expresion y e2 = (Cte 0)

-- cantidadDeSumaCero (simplificarProd e1 (Cte 0)) = cantidadDeSumaCero e1 * cantidadDeSumaCero (Cte 0) ?

-- LI)

-- cantidadDeSumaCero (simplificarProd e1 (Cte 0))
--                    ---------------------------
-- =                                       def simplificarProd
-- cantidadDeSumaCero (Cte 0)
-- --------------------------
-- =                                       def cantidadDeSumaCero
-- 0

-- LD)

-- cantidadDeSumaCero e1 * cantidadDeSumaCero (Cte 0)
--                         --------------------------
-- =                                       def cantidadDeSumaCero
-- cantidadDeSumaCero e1 * 0
-- -------------------------
-- =                                       0 es absorbente
-- 0

-- Caso3 = e1 es cualquier otra expresion y e2 = (Cte 1)

-- cantidadDeSumaCero (simplificarProd e1 (Cte 1)) = cantidadDeSumaCero e1 * cantidadDeSumaCero (Cte 1) ?

-- LI)

-- cantidadDeSumaCero (simplificarProd e1 (Cte 1))
--                     --------------------------
-- =                                       def simplificarProd
-- cantidadDeSumaCero e1

-- LD)

-- cantidadDeSumaCero e1 * cantidadDeSumaCero (Cte 1)
--                         --------------------------
-- =                                       def cantidadDeSumaCero
-- cantidadDeSumaCero e1 * 1
-- -------------------------
-- =                                       1 es neutro en multiplicacion
-- cantidadDeSumaCero e1


-- Caso4 = e1 = (Cte 1) y e2 es cualquier otra expresion

-- cantidadDeSumaCero (simplificarProd (Cte 1) e2) = cantidadDeSumaCero (Cte 1) * cantidadDeSumaCero e2 ?

-- LI)

-- cantidadDeSumaCero (simplificarProd (Cte 1) e2)
--                     --------------------------
-- =                                       def simplificarProd
-- cantidadDeSumaCero e2

-- LD)

-- cantidadDeSumaCero (Cte 1) * cantidadDeSumaCero e2
-- --------------------------
-- =                                       def cantidadDeSumaCero
-- cantidadDeSumaCero 1 * cantidadDeSumaCero e2
-- --------------------
-- =                                       1 es neutro en multiplicacion
-- cantidadDeSumaCero e2


-- Caso5 = e1 y e2 son cualquier expresion

-- cantidadDeSumaCero (simplificarProd e1 e2) = cantidadDeSumaCero e1 * cantidadDeSumaCero e2 ?

-- LI)

-- cantidadDeSumaCero (simplificarProd e1 e2)
--                    -----------------------
-- =                                       def simplificarProd
-- cantidadDeSumaCero (Prod e1 e2)
-- -------------------------------
-- =                                       def cantidadDeSumaCero
-- cantidadDeSumaCero e1 * cantidadDeSumaCero e2

-- LD)

-- cantidadDeSumaCero e1 * cantidadDeSumaCero e2

Lema: distr-sumacero
Prop)por ppio de extensionalidad
para todo e1. para todo e2. cantidadDeSumaCero (simplificarSuma e1 e2) = cantidadDeSumaCero e1 + cantidadDeSumaCero e2 ?

voy a demostrar por casos:

Caso 1: e1 = (Cte 0) y e2 != (Cte 0)

cantidadDeSumaCero (simplificarSuma (Cte 0) (Cte 0)) = cantidadDeSumaCero (Cte 0) + cantidadDeSumaCero (Cte 0) ?

Caso 2: e1 != (Cte 0) y e2 = (Cte 0)

cantidadDeSumaCero (simplificarSuma (Cte 0) e2) = cantidadDeSumaCero (Cte 0) + cantidadDeSumaCero e2 ?

Caso 3: e1 != (Cte 0) y e2 != (Cte 0)

cantidadDeSumaCero (simplificarSuma e1 e2) = cantidadDeSumaCero e1 + cantidadDeSumaCero e2 ?

Caso 1:

I)

cantidadDeSumaCero (simplificarSuma (Cte 0) m)
                    -------------------------------
=                       def simplificarSuma
cantidadDeSumaCero m

D)

cantidadDeSumaCero (Cte 0) + cantidadDeSumaCero m
-------------------------
=                       def de cantidadDeSumaCero
0 + cantidadDeSumaCero m
------------------------
=                       0 es el neutro
cantidadDeSumaCero m


Caso 2:

I)

cantidadDeSumaCero (simplificarSuma n (Cte 0))
                    --------------------------
=                       def simplificarSuma
cantidadDeSumaCero n

D)

cantidadDeSumaCero n + cantidadDeSumaCero (Cte 0) ?
                        ---------------------
=                       def de cantidadDeSumaCero
cantidadDeSumaCero n + 0
----------------------
=                       0 es el neutro
cantidadDeSumaCero n 

Caso 3:

I)

cantidadDeSumaCero (simplificarSuma e1 e2) 
                   -----------------------
=                       def de simplificarSuma
cantidadDeSumaCero (Suma e1 e2)
-------------------------------
=                       def de cantidadDeSumaCero
analizarSumas e1 e2 + cantidadDeSumaCero e1 + cantidadDeSumaCero e2
-------------------
=                       def de analizarSumas
0 + cantidadDeSumaCero e1 + cantidadDeSumaCero e2
-------------------------------------------------
=                       cero es el neutro
cantidadDeSumaCero e1 + cantidadDeSumaCero e2

D)

cantidadDeSumaCero e1 + cantidadDeSumaCero e2

-- b

-- i 
evalExpA . es2ExpA = evalES ?
para todo es. (evalExpA . es2ExpA) es = evalES es ?
por def de (.)
para todo es. evalExpA (es2ExpA es) = evalES es?
sea exps de tipo ExpS, por ppio de induccion en la estructura de exps, es equivalente demostrar que:

Caso Base: exps = (CteS n)

evalExpA (es2ExpA (CteS n)) = evalES (CteS n) ?

Caso Inductivo 1: exps = SumS e1 e2

HI1) evalExpA (es2ExpA e1) = evalES e1 !
HI2) evalExpA (es2ExpA e2) = evalES e2 !
TI) evalExpA (es2ExpA (SumS e1 e2)) = evalES (SumS e1 e2) ?

Caso Inductivo 2: exps = ProdS e1 e2

HI1) evalExpA (es2ExpA e1) = evalES e1 !
HI2) evalExpA (es2ExpA e2) = evalES e2 !
TI) evalExpA (es2ExpA (ProdS e1 e2)) = evalES (ProdS e1 e2) ?

Caso Base:

I)

evalExpA (es2ExpA (CteS n))
         -----------------
=               def de es2ExpA
evalExpA (Cte (evalN n))
-------------------------
=               def de evalExpA
evalN n

D)

evalES (CteS n)
---------------
=               def de evalES
evalN n

Caso Inductivo 1:

I)

evalExpA (es2ExpA (SumS e1 e2))
          --------------------
=               def de es2ExpA
evalExpA (SumS (es2ExpA e1) (es2ExpA e2))
---------------------------------------------
=               def de evalExpA
evalExpA (es2ExpA e1) + evalExpA (es2ExpA e1)
--------------------
=               HI1
evalES e1 + evalExpA (es2ExpA e2)
            ---------------------
=               HI2
evalES e1 + evalES e2


D)

evalES (SumS e1 e2)
-------------------
=               def de evalES
evalES e1 + evalES e2

Caso Inductivo 2:

I)

evalExpA (es2ExpA (ProdS e1 e2))
         ----------------------
=               def de es2ExpA
evalExpA (Prod (es2ExpA e1) (es2ExpA e2))
-----------------------------------------
=               def evalExpA
evalExpA (es2ExpA e1) * evalExpA (es2ExpA e2)

D)

evalES (ProdS e1 e2)
--------------------
=               def de evalES
evalES e1 * evalES e2
---------
=               HI1
evalExpA (es2ExpA e1) * evalES e2
                        ---------
=               HI2
evalExpA (es2ExpA e1) * evalExpA (es2ExpA e2)

-- iii

es2ExpA . expA2es = id 
Dem) por ppio de extensionalidad
para todo ea. (es2ExpA . expA2es) ea = id ea ?
por def de (.)
para todo ea. es2ExpA (expA2es ea) = id ea ?
por def de id
 es2ExpA (expA2es ea) = ea ?
sea exp un elemento de tipo ExpA, por ppio de inducción sobre la estructura de exp, es equivalente demostrar que:

Caso Base: exp = (Cte n)

es2ExpA (expA2es (Cte n)) = id (Cte n) ?

Caso Inductivo 1: exp = (Suma e1 e2)

HI1) es2ExpA (expA2es e1) = e1 !
HI2) es2ExpA (expA2es e2) = e2 !
TI)  es2ExpA (expA2es (Suma e1 e2)) = Suma e1 e2 ?

Caso Inductivo 2: exp = (Prod e1 e2)

HI1) es2ExpA (expA2es e1) = e1 ! 
HI2) es2ExpA (expA2es e2) = e2 !
TI) es2ExpA (expA2es (Prod e1 e2)) = (Prod e1 e2) ?

Caso Base:

I)

es2ExpA (expA2es (Cte n))
         ---------------
=       def de expA2es
es2ExpA (Ctes (int2N n))
------------------------
=       def de es2ExpA
Cte (evalN (int2N n))

D) 

Cte n 

Caso Inductivo 1: 

I)

es2ExpA (expA2es (Suma e1 e2))
         -------------------
=               def de expA2es
es2ExpA (SumS (expA2es e1) (expA2es e2))
-----------------------------------------
=               def de es2ExpA
Suma (es2ExpA (expA2es e1)) (es2ExpA (expA2es e2))
      --------------------
=               H1
Suma e1 (es2ExpA (expA2es e2))
        ---------------------
=               H2
Suma e1 e2

D)

Suma e1 e2 

Caso Inductivo 2:

I)

es2ExpA (expA2es (Prod e1 e2))
         --------------------
=               def de expA2es
es2ExpA (ProdS (expA2es e1) (expA2es e2))
-----------------------------------------
=               def de es2ExpA
Prod (es2ExpA (expA2es e1)) (es2ExpA (expA2es e2))
     ---------------------
=               HI1
Prod e1 (es2ExpA (expA2es e2))
        ---------------------
=               HI2
Prod e1 e2

D)

(Prod e1 e2)