data NExp = Var Variable | NCte Int | NBop NBinOp NExp NExp deriving (Show)

data NBinOp = Add | Sub | Mul | Div | Mod | Pow deriving (Show)

type Variable = String

ejemplo5 :: NExp
ejemplo5 = NBop Mul (NBop Add (Var "x") (NCte 2)) (NBop Sub (Var "y") (NCte 5))

-- Ejemplo 5: Expresión con potencia y módulo
ejemplo6 :: NExp
ejemplo6 = NBop Mod (NBop Pow (NCte 2) (NCte 3)) (NCte 5)

ej3 = cfNE (NBop Add (NCte 2) (NCte 3))

ej4 = cfNE (NBop Mul (NBop Add (NCte 3) (NCte 4)) (Var "y"))

ej5 = cfNE (NBop Mod (NBop Pow (NCte 2) (NCte 3)) (NCte 5))


-- 1) definir:

cantOfCtes :: NExp -> Int
cantOfCtes (Var v)                   = 0
cantOfCtes (NCte n)                  = 0
cantOfCtes (NBop nbinop nexp1 nexp2) = analizarBinop nexp1 nexp2 + cantOfCtes nexp1 + cantOfCtes nexp2

analizarBinop :: NExp -> NExp -> Int
analizarBinop (NCte n) (NCte m) = 1
analizarBinop _ _               = 0

cfNE :: NExp -> NExp -- aplica constant folding
cfNE (Var v)                   = (Var v)
cfNE (NCte n)                  = (NCte n)
cfNE (NBop nbinop nexp1 nexp2) = folding nbinop (cfNE nexp1) (cfNE nexp2)

folding :: NBinOp -> NExp -> NExp -> NExp
folding Add (NCte n) (NCte m) = NCte (n + m)
folding Sub (NCte n) (NCte m) = NCte (n - m)
folding Mul (NCte n) (NCte m) = NCte (n * m)
folding Div (NCte n) (NCte m) = NCte (n `div` m)
folding Mod (NCte n) (NCte m) = NCte (n `mod` m)
folding Pow (NCte n) (NCte m) = NCte (n ^ m)
folding op e1 e2              = NBop op e1 e2



-- 2) Demostrar: 

prop) cantOfCtes . cfNE = const 0 ?
Dem) por ppio de extensionalidad
para todo nexp. (cantOfCtes . cfNE) nexp = const 0 nexp ?
por def de (.)
para todo nexp. cantOfCtes (cfNE nexp) = const 0 nexp ?
por def de const
para todo nexp. cantOfCtes (cfNE nexp) = 0 ?
sea nxp de tipo NExp, por ppio de induccion sobre la estructura de nxp, es equivalente demostrar que:

CASO BASE 1 : nxp = Var v)

cantOfCtes (cfNE (Var v)) = 0 ?

CASO BASE 2 : nxp = NCte n)

cantOfCtes (cfNE (NCte n)) = 0 ?

CASO INDUCTIVO : nxp = NBop nbinop nexp1 nexp2)

HI1) cantOfCtes (cfNE nexp1) = 0 ?
HI2) cantOfCtes (cfNE nexp2) = 0 ?
TI) cantOfCtes (cfNE (NBop nbinop nexp1 nexp2)) = 0 ?


CASO BASE 1:

IZQ)

cantOfCtes (cfNE (Var v))
            ------------
=           def cfNE
cantOfCtes (Var v)
------------------
=           def cantOfCtes
0

DER)

0

CASO BASE 2:

LI)

cantOfCtes (cfNE (NCte n))
            -------------
=           def cfNE
cantOfCtes (NCte n)
-------------------
=           def cantOfCtes
0

LD)

0

CASO INDUCTIVO :

voy a proceder a resolverlo por casos:

Caso 1: nexp1 = (NCte n) , nexp2 = (NCte m)

LI)

cantOfCtes (cfNE (NBop nbinop (NCte n) (NCte m)))
            -----------------------------------
=                   def cfNE
cantOfCtes ( folding nbinop (cfNE (NCte n)) (cfNE (NCte m)) 
                            ---------------  --------------
=                   def cfNE
cantOfCtes (folding nbinop (NCte n) (NCte m))
            --------------------------------
=                   def folding
cantOfCtes (NCte (evalNbinop nbinop n m))
----------------------------------------
=                   def cantOfCtes
0

Caso 2: nexp1 != (NCte n), nexp2 != (NCte m)

LI)

cantOfCtes (cfNE (NBop nbinop nexp1 nexp2))
            ------------------------------
=                   def cfNE
cantOfCtes (folding nbinop (cfNE nexp1) (cfNE nexp2))
            ----------------------------------------
=                   def folding
cantOfCtes (NBop nbinop (cfNE nexp1) (cfNE nexp2))
-------------------------------------------------
=                   def cantOfCtes
analizarBinop (cfNE nexp1) (cfNE nexp1) + cantOfCtes(cfNE nexp1) + cantOfCtes(cfNE nexp2)
                                          ----------------------   ----------------------
=                   HI1, HI2
analizarBinop (cfNE nexp1) (cfNE nexp1) + 0 + 0
---------------------------------------
=                   def analizarBinop
0 + 0 + 0
----------
=                   def +
0

cantOfCtes :: NExp -> Int
cantOfCtes (Var v)                   = 0
cantOfCtes (NCte n)                  = 0
cantOfCtes (NBop nbinop nexp1 nexp2) = analizarBinop nexp1 nexp2 + cantOfCtes nexp1 + cantOfCtes nexp2

analizarBinop :: NExp -> NExp -> Int
analizarBinop (NCte n) (NCte m) = 1
analizarBinop _ _               = 0

cfNE :: NExp -> NExp -- aplica constant folding
cfNE (Var v)                   = (Var v)
cfNE (NCte n)                  = (NCte n)
cfNE (NBop nbinop nexp1 nexp2) = folding nbinop (cfNE nexp1) (cfNE nexp2)

folding :: NBinOp -> NExp -> NExp -> NExp
folding nbinop (NCte n) (NCte m) = NCte (evalNbinop nbinop n m)
folding nbinop nexp1 nexp2       = NBop nbinop nexp1 nexp2

evalNbinop :: NBinOp -> Int -> Int -> Int
evalNbinop Add = (+)
evalNbinop Sub = (-)
evalNbinop Mul = (*)
evalNbinop Div = div
evalNbinop Mod = mod
evalNbinop Pow = (^)

