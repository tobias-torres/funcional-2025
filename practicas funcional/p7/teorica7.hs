-- Necesito una opinion con una demostracion: 
-- para todo f. subst const f = id
-- para todo f. para todo g. subst const f g = id g
-- sea g' una funcion, y un elemento cualquiera. Se vera que subst const f g' = id g'


-- Lado Izq)

-- subst const f g'
-- ---------------
-- =                       def de subst
-- const g' (f g')
-- ---------------
-- =                       def de const
-- g'

-- Lado Der)

-- id g'
-- ----
-- =                       def id
-- g'

-- 1)
-- a)
-- definicion inductiva del conjunto ABs:

-- regla 1: a tiene que estar en el conjunto ABs
-- regla 2: b tiene que estar en el conjunto ABs
-- regla 3: si e esta en ABs, entonces ea esta en ABs
-- regla 4: si e esta en ABs, entonces eb esta en ABs

-- b)
-- definicion inductiva del conjunto BAs:

-- regla 1: a pertenece al conjunto BAs
-- regla 2: si e esta en BAs, entonces ae esta en BAs
-- regla 3: si e esta en BAs, entonces be esta en BAs

-- c)
-- definicion inductiva del conjunto ABBs:

-- regla 1: a pertenece al conjunto ABBs
-- regla 2: bb pertenece al conjunto ABBs
-- regla 3: si e esta en ABBs, entonces ea esta en ABBs
-- regla 4: si e esta en ABBs, entonces ebb esta en ABBs

-- 2)

data ABs = A | B | AgregaA ABs | AgregaB ABs deriving (Show)

-- expresionABs :: ABs
-- expresionABs = AddingA (AddingB A)

-- data BAs = A | AgregaA BAs | AgregaB BAs deriving (Show)

-- data ABBs = A | BB | AgregaA ABBs | AgregaBB ABBs deriving (Show)

-- 3)
-- a

estrABs_a = A

estrABs_b = B

estrABs_ab = AgregaA B

estrABs_ba = AgregaB A

estrABs_aa = AgregaA A

estrABs_aab = AgregaA (AgregaA B)

estrABs_bb = AgregaB B

estrABs_bba = AgregaB (AgregaB A)

-- b

-- estrBAs_a = A

-- estrBAs_ba = AgregaB A

-- estrBAs_aa = AgregaA A

-- estrBAs_baa = AgregaB (AgregaA A)

-- estrBAs_bba = AgregaB (AgregaB A)

-- c

-- estrABBs_a = A

-- estrABBs_abb = AgregaA BB

-- estrABBs_bb = BB

-- estrABBs_bba = AgregaBB A

-- estrABBs_bbabbbb = AgregaBB (AgregaA (AgregaBB BB))

-- estrABBs_aabbbbaaabb = AgregaA (AgregaA (AgregaBB (AgregaBB (AgregaA (AgregaA (AgregaA BB))))))

-- 4

-- data BAs = A | AgregaA BAs | AgregaB BAs deriving (Show)

-- data ABBs = A | BB | AgregaA ABBs | AgregaBB ABBs deriving (Show)

printABs :: ABs -> String
printABs A             = "a"
printABs B             = "b"
printABs (AgregaA abs) = "a" ++ printABs abs
printABs (AgregaB abs) = "b" ++ printABs abs

printBAs :: BAs -> String
printBAs A             = "A"
printBAs (AgregaA bas) = "A" ++ printBAs bas
printBAs (AgregaB bas) = "B" ++ printBAs bas

printABBs :: ABBs -> String
printABBs A               = "A"
printABBs BB              = "BB"
printABBs (AgregaA abbs)  = "A" ++ printABBs abbs
printABBs (AgregaBB abbs) = "BB" ++ printABBs abbs