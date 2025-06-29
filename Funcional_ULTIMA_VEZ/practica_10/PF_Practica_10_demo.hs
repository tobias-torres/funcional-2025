## Demo Practica 10

## Ejercicio 1

i) Prop: evalBExp . cfBExp = evalBExp ?

Dem: por ppio de extensionalidad.
para todo nexp. (evalBExp . cfBExp) nexp = evalBExp nexp ?
por def de (.)
para todo nexp. evalBExp (cfBExp nexp) = evalBExp nexp ?
sea nxp de tipo NExp. por ppio de induccion sobre la estructura de nxp, es equivalente demostrar que:

CASO BASE 1: nxp = (Var v)

evalBExp (cfBExp (Var v)) = evalBExp (Var v) ?

CASO BASE 2: nxp = (NCte n)

evalBExp (cfBExp (NCte n)) = evalBExp (NCte n) ?

CASO INDUCTIVO: nxp = NBOp binop nexp1 nexp2)

HI1) evalBExp (cfBExp nexp1) = evalBExp nexp1 !
HI2) evalBExp (cfBExp nexp2) = evalBExp nexp2 !
TI) evalBExp (cfBExp (NBOp binop nexp1 nexp2)) = evalBExp (NBOp binop nexp1 nexp2) ?

CASO BASE 1:

LI)

evalBExp (cfBExp (Var v))
         ---------------
=               def cfBExp
evalBExp (Var v)

LD)

evalBExp (Var v)


CASO BASE 2:

LI)

evalBExp (cfBExp (NCte n))
         -----------------
=               def cfBExp
evalBExp (NCte n)

LD)

evalBExp (NCte n)


Caso inductivo:

Caso binop = ADD

LI)

evalBExp (cfBExp (NBOp ADD nexp1 nexp2))
          -------------------------------
=               def cfBExp
evalBExp (simplificarCfNExp ADD (cfNExp nexp1) (cfNExp nexp2))
          ---------------------------------------------------
=               def simplificarCfNExp
evalBExp (evalAdd (cfNExp nexp1) (cfNExp nexp2))
------------------------------------------------
=               lema add-distributivo
evalBExp (cfBExp nexp1) + evalBExp (cfBExp nexp2)



LD)

evalBExp (NBOp ADD nexp1 nexp2)
-------------------------------
=               def evalBExp
evalNbinop ADD (evalBExp nexp1) (evalBExp nexp2)
-----------------------------------------------
=               def evalNbinop
(evalBExp nexp1) + (evalBExp nexp2)
---------------     ---------------
=               HI1, HI2
evalBExp (cfBExp nexp1) + evalBExp (cfBExp nexp2)

LEMA: add-distributivo
por ppio de extensionalidad
para todo nxp1. para todo nxp2. evalBExp (evalAdd nxp1 nxp2) = evalBExp nxp1 + evalBExp nxp2 ?
voy a demostrar por casos:

Caso 1: nxp1 = (NCte 0)

LI)

evalBExp (evalAdd (NCte 0) nxp2)
         ----------------------
=               def evalAdd
evalBExp nxp2

LD)

evalBExp (NCte 0) + evalBExp nxp2
-----------------
=               def evalBExp
0 + evalBExp nxp2
-----------------
=               + es neutro de suma
evalBExp nxp2


Caso 2: nxp2 = (NCte 0)

LI)

evalBExp (evalAdd nxp1 (NCte 0))
         ----------------------
=               def evalAdd
evalBExp nxp1

LD)

evalBExp nxp1 + evalBExp (NCte 0)
                -----------------
=               def evalBExp
evalBExp nxp1 + 0
-----------------
=               + es neutro de suma
evalBExp nxp1


Caso 3: nxp1 = (NCte n) && nxp2 = (NCte m)

LI)

evalBExp (evalAdd (NCte n) (NCte m))
         --------------------------
=               def evalAdd
evalBExp (NCte (n + m))
-----------------------
=               def evalBExp
(n + m)

LD)

evalBExp (NCte n) + evalBExp (NCte m)
-----------------   -----------------
=               def de evalBExp dos veces
n + m

Caso 4: nxp1 /= (Cte 0) && nxp2 /= (Cte 0)

LI)

evalBExp (evalAdd nxp1 nxp2)
          -----------------
=               def evalAdd
evalBExp (NBOp Add nxp1 nxp2)
-----------------------------
=               def evalBExp
evalNbinop Add (evalBExp nxp1) (evalBExp nxp2)
----------------------------------------------
=               def evalNbinop
(evalBExp nxp1) + (evalBExp nxp2)

LD)

evalBExp nxp1 + evalBExp nxp2