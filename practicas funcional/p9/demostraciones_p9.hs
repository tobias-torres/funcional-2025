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

iv. evalEA . expA2ea = evalExpA