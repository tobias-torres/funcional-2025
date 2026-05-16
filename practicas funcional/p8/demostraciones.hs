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


D)

evalN (S n) * evalN m2
-----------
=               def de evalN
1 + evalN n * evalN m2
    ------------------
=               HI
1 + evalN (prodN n m2)


HI) evalN (prodN n m2) = evalN n * evalN m2 !

-- iii.  int2N . evalN = id 
-- iv.  evalN . int2N = id 