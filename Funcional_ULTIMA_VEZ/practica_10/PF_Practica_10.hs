data NExp = Var Variable | NCte Int | NBOp NBinOp NExp NExp

data NBinOp = Add | Sub | Mul | Div | Mod | Pow

type Variable = String

TAD Memoria cuya interfaz es la siguiente:

○ enBlanco :: Memoria, que describe una memoria vacía.

○ cuantoVale :: Variable -> Memoria -> Maybe Int, que describe
el número asociado a la variable dada en la memoria dada.

○ recordar :: Variable -> Int -> Memoria -> Memoria, que la
memoria resultante de asociar el número dado a la variable dada en la memoria dada.

○ variables :: Memoria -> [Variable], que describe las variables que
la memoria recuerda

evalBExp :: NExp -> Memoria -> Int
evalBExp (Var v) m                   = fromJust (cuantoVale v m)
evalBExp (NCte n) m                  = n
evalBExp (NBOp nbinop nexp1 nexp2) m = evalNbinop nbinop (evalBExp nexp1 m) (evalBExp nexp2 m)

evalNbinop :: NBinOp -> Int -> Int -> Int
evalNbinop Add = (+)
evalNbinop Sub = (-)
evalNbinop Mul = (*)
evalNbinop Div = div
evalNbinop Mod = mod
evalNbinop Pow = (^)

cfNExp :: NExp -> NExp
cfNExp (Var v)                   = (Var v)
cfNExp (NCte n)                  = (NCte n)
cfNExp (NBOp nbinop nexp1 nexp2) = simplificarCfNExp nbinop (cfNExp nexp1) (cfNExp nexp2)

simplificarCfNExp :: NBinOp -> NExp -> NExp -> NExp
simplificarCfNExp Add nexp1 nexp2 = evalAdd nexp1 nexp2
simplificarCfNExp Mul nexp1 nexp2 = evalMul nexp1 nexp2

evalAdd :: NExp -> NExp -> NExp
evalAdd (NCte 0) nexp     = nexp
evalAdd nexp (NCte 0)     = nexp
evalAdd (NCte n) (NCte m) = NCte (n+m)
evalAdd nexp1 nexp2       = NBOp Add nexp1 nexp2

evalMul :: NExp -> NExp -> NExp
evalMul (NCte 0) _        = NCte 0
evalMul _ (NCte 0)        = NCte 0
evalMul exp (NCte 1)      = exp
evalMul (NCte 1) exp      = exp
evalMul (NCte n) (NCte m) = NCte (n*m)
evalMul expa1 expa2       = NBOp Mul expa1 expa2

