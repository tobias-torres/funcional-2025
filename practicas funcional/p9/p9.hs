data EA = Const Int | BOp BinOp EA EA deriving (Show)

data BinOp = Sum | Mul deriving (Show)

data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA

-- que describe el número que resulta de evaluar la cuenta representada por la expresión aritmética dada.
evalEA :: EA -> Int
evalEA (Const n)         = n
evalEA (BOp binop e1 e2) = evaluar binop (evalEA e1) (evalEA e2)

evaluar :: BinOp -> Int -> Int -> Int
evaluar Sum n m = n + m
evaluar Mul n m = n * m

-- que describe una expresión aritmética representada con el tipo ExpA, cuya estructura y significado son los mismos que la dada.
ea2ExpA :: EA -> ExpA
ea2ExpA (Const n)         = Cte n
ea2ExpA (BOp binop e1 e2) = armarExpA binop (ea2ExpA e1) (ea2ExpA e2) 

armarExpA :: BinOp -> ExpA -> ExpA -> ExpA
armarExpA Sum e1 e2 = Suma e1 e2
armarExpA Mul e1 e2 = Prod e1 e2

-- que describe una expresión aritmética representada con el tipo EA, cuya estructura y significado son los mismos que la dada.
expA2ea :: ExpA -> EA
expA2ea (Cte n)      = Const n
expA2ea (Suma e1 e2) = BOp Sum (expA2ea e1) (expA2ea e2)
expA2ea (Prod e1 e2) = BOp Mul (expA2ea e1) (expA2ea e2)

