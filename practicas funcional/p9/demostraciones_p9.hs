demostrar la siguiente propiedad:

i. ea2ExpA . expA2ea = id
por ppio de extensionalidad
para todo exp. (ea2ExpA . expA2ea) exp = id exp ?
por def de (.)
para todo exp. ea2ExpA (expA2ea exp) = id exp ?
por def de id 
para todo exp. ea2ExpA (expA2ea exp) = exp ?
sea expa de tipo ExpA. por ppio de induccion sobre la estructura de expa, es equivalente demostrar que:

Caso Base expa = Cte n)

ea2ExpA (expA2ea (Cte n)) = (Cte n) ?

Caso Inductivo 1: expa = (Suma e1 e2)

HI1) ea2ExpA (expA2ea e1) = e1 !
HI2) ea2ExpA (expA2ea e2) = e2 !
TI) ea2ExpA (expA2ea (Suma e1 e2)) = (Suma e1 e2) ?

Caso Inductivo 2: expa = (Prod e1 e2)

HI1) ea2ExpA (expA2ea e1) = e1 !
HI2) ea2ExpA (expA2ea e2) = e2 !
TI) ea2ExpA (expA2ea (Prod e1 e2)) = (Prod e1 e2) ?

Caso Base:

I)

ea2ExpA (expA2ea (Cte n))
        -----------------
=           def expA2ea
ea2ExpA (Const n)
-----------------
=           def ea2ExpA
Cte n

Caso Inductivo 1:

I)

ea2ExpA (expA2ea (Suma e1 e2))
        ---------------------
=           def de expA2ea
ea2ExpA (BOp Sum (expA2ea e1) (expA2ea e2))
-------------------------------------------
=           def de ea2ExpA
armarExpA Sum (ea2ExpA (expA2ea e1)) (ea2ExpA (expA2ea e2))
-----------------------------------------------------------
=           def de armarExpA
Suma (ea2ExpA (expA2ea e1)) (ea2ExpA (expA2ea e2))
     ---------------------  ----------------------
=           HI1, HI2
Suma e1 e2

Caso Inductivo 2:

I)

ea2ExpA (expA2ea (Prod e1 e2))
        ---------------------
=           def de expA2ea
ea2ExpA (BOp Mul (expA2ea e1) (expA2ea e2))
-------------------------------------------
=           def de ea2ExpA
armarExpA Mul (ea2ExpA (expA2ea e1)) (ea2ExpA (expA2ea e2))
-----------------------------------------------------------
=           def de armarExpA
Prod (ea2ExpA (expA2ea e1)) (ea2ExpA (expA2ea e2))
     ---------------------  ----------------------
=           HI1, HI2
Prod e1 e2


ii.
Prop) expA2ea . ea2ExpA = id ?
Dem) por ppio de extensionalidad
para todo ea. (expA2ea . ea2ExpA) ea = id ea ?
por def de (.)
para todo ea. expA2ea (ea2ExpA ea) = id ea ?
por def de id
para todo ea. expA2ea (ea2ExpA ea) = ea ?
sea ea' de tipo EA. por ppio de induccion en la estructura de ea', es equivalente demostrar que:

Caso Base: ea = Const n)

expA2ea (ea2ExpA ea) = Const n ?

Caso Inductivo : ea = (BOp binop e1 e2)

subcaso de binop: Sum

HI1) expA2ea (ea2ExpA e1) = e1 !
HI2) expA2ea (ea2ExpA e2) = e2 !
TI) expA2ea (ea2ExpA (BOp Sum e1 e2)) = (BOp Sum e1 e2) ?

subcaso de binop: Mul

HI1) expA2ea (ea2ExpA e1) = e1 !
HI2) expA2ea (ea2ExpA e2) = e2 !
TI) expA2ea (ea2ExpA (BOp Mul e1 e2)) = (BOp Mul e1 e2) ?

Caso Base:

I)
expA2ea (ea2ExpA (Const n))
        ------------------
=           def ea2ExpA
expA2ea (Cte n)
---------------
=           def expA2ea
Const n

Caso Inductivo subcaso: Sum

I)

expA2ea (ea2ExpA (BOp Sum e1 e2))
        --------------------------
=           def de ea2ExpA
expA2ea (armarExpA Sum (ea2ExpA e1) (ea2ExpA e2))
        ----------------------------------------
=           def armarExpA
expA2ea (Suma (ea2ExpA e1) (ea2ExpA e2))
----------------------------------------
=           def expA2ea
BOp Sum (expA2ea(ea2ExpA e1)) (expA2ea(ea2ExpA e2))
        --------------------   -------------------
=           HI1, HI2
BOp Sum e1 e2


Caso Inductivo subcaso: Mul

I)

expA2ea (ea2ExpA (BOp Mul e1 e2))
        --------------------------
=           def de ea2ExpA
expA2ea (armarExpA Mul (ea2ExpA e1) (ea2ExpA e2))
        ----------------------------------------
=           def armarExpA
expA2ea (Prod (ea2ExpA e1) (ea2ExpA e2))
----------------------------------------
=           def expA2ea
BOp Prod (expA2ea(ea2ExpA e1)) (expA2ea(ea2ExpA e2))
        --------------------   -------------------
=           HI1, HI2
BOp Prod e1 e2




iii. evalExpA . ea2ExpA = evalEA
por ppio de extensionalidad.
para todo ea1. (evalExpA . ea2ExpA) ea1 = evalEA ea1?
por def de (.)
para todo ea1. evalExpA (ea2ExpA ea1) = evalEA ea1 ?
sea ea un elemento de tipo EA, por ppio de induccion sobre la estructura de ea, es equivalente demostrar que:

CASO Base: ea = Const n)

evalExpA (ea2ExpA (Const n)) = evalEA (Const n) ?

Caso Inductivo: ea = (BOp binop e1 e2)

subcaso de binop: Sum

HI1) evalExpA (ea2ExpA e1) = evalEA e1 !
HI2) evalExpA (ea2ExpA e2) = evalEA e2 !
TI) evalExpA (ea2ExpA (BOp Sum e1 e2)) = evalEA (BOp Sum e1 e2) ?

Caso Inductivo: ea = (BOp binop e1 e2)

subcaso de binop: Mul

HI1) evalExpA (ea2ExpA e1) = evalEA e1 !
HI2) evalExpA (ea2ExpA e2) = evalEA e2 !
TI) evalExpA (ea2ExpA (BOp Mul e1 e2)) = evalEA (BOp Mul e1 e2) ?

Caso Base:

I)

evalExpA (ea2ExpA (Const n))
         -------------------
=                               def de ea2ExpA
evalExpA (Cte n)
----------------
=                               def de evalExpA
n

D)

evalEA (Const n)
----------------
=                               def de evalEA
n

Caso Inductivo:
subcaso de binop: Sum

I)

evalExpA (ea2ExpA (BOp Sum e1 e2))
         ---------------------------
=                               def de ea2ExpA
evalExpA (armarExpA Sum (ea2ExpA e1) (ea2ExpA e2) )
         -----------------------------------------
=                               def de armarExpA
evalExpA (Suma (ea2ExpA e1) (ea2ExpA e2))
----------------------------------------
=                               def de evalExpA
evalExpA (ea2ExpA e1) + evalExpA (ea2ExpA e2)


D)

evalEA (BOp Sum e1 e2)
----------------------
=                               def de evalEA
evaluar Sum (evalEA e1) (evalEA e2)
-----------------------------------
=                               def de evaluar
(evalEA e1) + (evalEA e2)
-----------    ----------
=                               HI1, HI2
evalExpA (ea2ExpA e1) + evalExpA (ea2ExpA e2)


subcaso de binop: Mul

I)

evalExpA (ea2ExpA (BOp Mul e1 e2))
         ------------------------
=                               def de ea2ExpA
evalExpA (armarExpA Mul (ea2ExpA e1) (ea2ExpA e2))
          ---------------------------------------
=                               def de armarExpA
evalExpA (Prod (ea2ExpA e1) (ea2ExpA e2))
-----------------------------------------
=                               def de evalExpA
evalExpA (ea2ExpA e1) * evalExpA (ea2ExpA e2)
--------------------    ---------------------
=                               HI1, HI2
(evalEA e1) * (evalEA e2)

D)

evalEA (BOp Mul e1 e2)
----------------------
=                               def de evalEA
evaluar Mul (evalEA e1) (evalEA e2)
-----------------------------------
=                               def de evaluar
(evalEA e1) * (evalEA e2)


iv. evalEA . expA2ea = evalExpA
por ppio de extensionalidad
para todo exp. (evalEA . expA2ea) exp= evalExpA exp ?
por def de (.)
para todo exp. evalEA (expA2ea exp) = evalExpA exp ?
sea e1 de tipo ExpA, por ppio de induccion en la estructura de e1, es equivalente demostrar que:

Caso Base: e1 = (Cte n)

evalEA (expA2ea (Cte n)) = evalExpA (Cte n) ?

Caso Inductivo 1: e1 = (Suma exp1 exp2)

HI1) evalEA (expA2ea exp1) = evalExpA exp1 !
HI2) evalEA (expA2ea exp2) = evalExpA exp2 !
TI) evalEA (expA2ea (Suma exp1 exp2)) = evalExpA (Suma exp1 exp2) ?

Caso Inductivo 2: e1 = (Prod exp1 exp2)

HI1) evalEA (expA2ea exp1) = evalExpA exp1 !
HI2) evalEA (expA2ea exp2) = evalExpA exp2 !
TI) evalEA (expA2ea (Prod exp1 exp2)) = evalExpA (Prod exp1 exp2) ?

Caso Base:

I)

evalEA (expA2ea (Cte n))
        ---------------
=                               def de expA2ea
evalEA (Const n)
----------------
=                               def de evalEA
n

D)

evalExpA (Cte n)
----------------
=                               def de evalExpA
n

Caso Inductivo 1:

I)

evalEA (expA2ea (Suma exp1 exp2))
        ------------------------
=                               def de expA2ea
evalEA (BOp Sum (expA2ea exp1) (expA2ea exp2))
----------------------------------------------
=                               def de evalEA
evaluar Sum (evalEA (expA2ea exp1)) (evalEA (expA2ea exp2))
-----------------------------------------------------------
=                               def de evaluar
(evalEA (expA2ea exp1)) + (evalEA (expA2ea exp2))

D)

evalExpA (Suma exp1 exp2)
------------------------
=                               def de evalExpA
evalExpA e1 + evalExpA e2
-----------   -----------
=                               HI1, HI2
(evalEA (expA2ea exp1)) + (evalEA (expA2ea exp2))

Caso Inductivo 2:

I)

evalEA (expA2ea (Prod exp1 exp2))
        ------------------------
=                               def de expA2ea
evalEA (BOp Mul (expA2ea exp1) (expA2ea exp2))
----------------------------------------------
=                               def de evalEA
evaluar Mul (evalEA (expA2ea exp1)) (evalEA (expA2ea exp2))
-----------------------------------------------------------
=                               def de evaluar
(evalEA (expA2ea exp1)) * (evalEA (expA2ea exp2))
----------------------    -----------------------
=                               HI1, HI2
evalExpA exp1 * evalExpA exp2

D)

evalExpA (Prod exp1 exp2)
-------------------------
=                               def de evalExpA
evalExpA exp1 * evalExpA exp2


----------------------------------------------------------------------------------------------------------------------------------------------

i. heightT = length . ramaMasLarga
por ppio de extensionalidad.
para todo tree. heightT tree = (length . ramaMasLarga) tree?
por def de (.)
para todo tree. heightT tree = length (ramaMasLarga tree) ?
sea t un arbol de tipo Tree a, por ppio de induccion sobre t, se vera que:

Caso Base: tree =EmptyT

heightT EmptyT = length (ramaMasLarga EmptyT) ?

Caso Inductivo: tree = NodeT x t1 t2)

HI1) heightT t1 = length (ramaMasLarga t1) ?
HI2) heightT t2 = length (ramaMasLarga t2) ?
TI) heightT (NodeT x t1 t2) = length (ramaMasLarga  (NodeT x t1 t2)) ?

Caso Base:

I)

heightT EmptyT
--------------
=                               def de heightT
0

D)

length (ramaMasLarga EmptyT)
        -------------------
=                               def de ramaMasLarga
length []
---------
=                               def de length
0


Caso Inductivo:

I)

heightT (NodeT x t1 t2)
-----------------------
=                               def de heightT
1 + max (heightT t1) (heightT t2)
    -----------------------------
=                               def de max
1 + if (heightT t1) > (heightT t2) then (heightT t1) else (heightT t2)
----------------------------------------------------------------------
=                               prop: f (if b then y else z) = if b then f y else f z
if (heightT t1) > (heightT t2) then 1 + (heightT t1) else 1 + (heightT t2)
                                        ------------          ------------
=                               HI1, HI2
if (heightT t1) > (heightT t2) then 1 + length (ramaMasLarga t1) else 1 + length (ramaMasLarga t2)


D)

length (ramaMasLarga  (NodeT x t1 t2))
        -----------------------------
=                               def de ramaMasLarga
length ( if length (ramaMasLarga t1) > length (ramaMasLarga t2) then x : ramaMasLarga t1 else x : ramaMasLarga t2)
            ---------------------------------------------------
=                               HI1, HI2
length (if heightT t1 > heightT t2 then x : ramaMasLarga t1 else x : ramaMasLarga t2)
-------------------------------------------------------------------------------------
=                               prop: f (if b then y else z) = if b then f y else f z
if heightT t1 > heightT t2 then length (x : ramaMasLarga t1) else length (x : ramaMasLarga t2)
                                ---------------------------       ----------------------------
=                               def de length
if heightT t1 > heightT t2 then 1 + length (ramaMasLarga t1) else 1 + length (ramaMasLarga t2)



ii. reverse . inOrder = inOrder . mirrorT
por ppio de extensionalidad
para todo tree. (reverse . inOrder) tree = (inOrder . mirrorT) tree ?
por def de (.) dos veces
para todo tree. reverse (inOrder tree) = inOrder (mirrorT tree) ?
sea t de tipo Tree a, por ppio de induccion sobre la estructura de t, es equivalente demostrar que:

Caso Base: t = EmptyT

reverse (inOrder EmptyT) = inOrder (mirrorT EmptyT) ?

Caso Inductivo: t = (NodeT x t1 t2)

HI1) reverse (inOrder t1) = inOrder (mirrorT t1) !
HI2) reverse (inOrder t2) = inOrder (mirrorT t2) !
TI) reverse (inOrder (NodeT x t1 t2)) = inOrder (mirrorT (NodeT x t1 t2)) ?

Caso Base:

I)

reverse (inOrder EmptyT)
        ----------------
=                       def inOrder
reverse []
----------
=                       def de reverse
[]

D)

inOrder (mirrorT EmptyT)
        ---------------
=                       def de mirrorT
inOrder EmptyT
--------------
=                       def inOrder
[]

Caso Inductivo:

I)

reverse (inOrder (NodeT x t1 t2))
        ------------------------
=                       def de inOrder
reverse (inOrder t1 ++ [x] ++ inOrder t2)
-----------------------------------------
=                       Lema-distrReverse
reverse (inOrder t2) ++ [x] ++ reverse (inOrder t1)

D)

inOrder (mirrorT (NodeT x t1 t2))
        ------------------------
=                       def de mirrorT
inOrder (NodeT x (mirrorT t2) (mirrorT t1))
-------------------------------------------
=                       def de inOrder
inOrder (mirrorT t2) ++ [x] ++ inOrder (mirrorT t1)
--------------------           --------------------
=                       HI1, HI2
reverse (inOrder t2) ++ [x] ++ reverse (inOrder t1)


def Lema-distrReverse
por ppio de extensionalidad
para todo xs. para todo ys. reverse (xs ++ [x] ++ ys) = reverse ys ++ [x] ++ reverse xs ?
sean ts e zs dos listas, por ppio de induccion sobre la estructura de ts, es equivalente demostrar que:

Caso Base: ts = []

reverse ([] ++ [x] ++ zs) = reverse zs ++ [x] ++ reverse [] ?

Caso Inductivo: ts = (x:xs')

HI) reverse (xs' ++ [x] ++ zs) = reverse zs ++ [x] ++ reverse xs' !
TI) reverse ((x:xs') ++ [x] ++ zs) = reverse zs ++ [x] ++ reverse (x:xs') ?

Caso Base:

I)

reverse ([] ++ [x] ++ zs)
               ----------
=                       def de ++
reverse ([] ++ x : ([] ++ zs))
                    ---------
=                       def de ++
reverse ([] ++ x : zs)
         ------------
=                       def de ++
reverse (x : zs)
----------------
=                       def de reverse
reverse zs ++ [x]



D)

reverse zs ++ [x] ++ reverse []
                     ----------
=                       def de reverse
reverse zs ++ [x] ++ []
              ---------
=                       def de ++
reverse zs ++ [x]

Caso Inductivo:

I)

reverse ((x:xs') ++ [x] ++ zs)
                    ---------
=                       def de ++
reverse ((x:xs') ++ x : ([] ++ zs))
                        ---------
=                       def de ++
reverse ((x:xs') ++ (x : zs))
        -------------------
=                       def de ++
reverse (x : (xs' ++ (x: zs)))
------------------------------
=                       def de reverse
reverse (xs' ++ (x:zs)) ++ [x]

D)

reverse zs ++ [x] ++ reverse (x:xs')
                      --------------
=                       def de reverse
reverse zs ++ [x] ++ (reverse xs') ++ [x]
----------------------------------
=                       HI
reverse (xs' ++ [x] ++ zs) ++ [x]
                ---------
=                       def de ++
reverse (xs' ++ x : [] ++ zs) ++ [x]
                    --------
=                       def de ++
reverse (xs' ++ (x : zs)) ++ [x]












