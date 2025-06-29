data Mapa = Equis Cofre | Recto Int Mapa | Giro Dir Mapa

data Cofre = Monedas Int | CorazonDe Pirata | Joyas Int | Nada

data Pirata = DavyJones | WillTurner

data Dir = Izq | Der

tesoroEn :: Mapa -> Cofre
tesoroEn (Equis c) = cavarYDar c
tesoroEn (Recto n m) = caminarYDar n (tesoroEn m)
tesoroEn (Giro d m) = girarYDar d (tesoroEn m)

comprimido :: Mapa -> Mapa
comprimido (Equis c) = Equis c
comprimido (Recto n m) = compRecto n (comprimido m)
comprimido (Giro d m) = Giro d (comprimido m)

compRecto :: Int -> Mapa -> Mapa
compRecto n (Recto n' m) = Recto (n+n') m
compRecto n m = Recto n m

cavarYDar c = c -- Implementaciones triviales
caminarYDar n c = c -- Podrían complicarse con un
girarYDar d c = c -- terreno y verificaciones…

-- Prop: ¿para todo x. tesoroEn (comprimido x) = tesoroEn x?

-- Dem: sea m un mapa. Por ind. en la estructura de m

-- tesoroEn (comprimido m) = tesoroEn m ?

-- Caso base, m = Equis c) 

-- ¿tesoroEn (comprimido (Equis c)) = tesoroEn (Equis c)?

-- Caso ind.1, m = Recto n m')

-- HI) ¡tesoroEn (comprimido m') = tesoroEn m'!
-- TI) ¿tesoroEn (comprimido (Recto n m')) = tesoroEn (Recto n m')?

-- Caso ind.2, m = Giro d m')

-- HI) ¡tesoroEn (comprimido m') = tesoroEn m'!
-- TI) ¿tesoroEn (comprimido (Giro d m')) = tesoroEn (Giro d m')?

-- --------------------------------------------------------------------------------------------------------

-- Caso Base, m = Equis c)

-- Lado Izq)

-- tesoroEn (comprimido (Equis c))
-- =                                               def comprimido
-- tesoroEn (Equis c)

-- Lado Der)

-- tesoroEn (Equis c)

-- --------------------------------------------------------------------------------------------------------

-- Caso ind.1, m = Recto n m')

-- Lado Izq)

-- tesoroEn (comprimido (Recto n m'))
--         -------------------------
-- =                                               def de comprimido
-- tesoroEn (compRecto n (comprimido m'))
-- =                                               Lema


-- Lado Der)

-- tesoroEn (Recto n m')
-- ---------------------
-- =                                               def tesoroEn
-- caminarYDar n (tesoroEn m')


--------------------------------------------------------------------------------------------------------

-- Caso ind.2, m = Giro d m')

-- Lado Izq)

-- tesoroEn (comprimido (Giro d m'))
--          -----------------------
-- =                                               def comprimido
-- tesoroEn (Giro d (comprimido m'))
-- --------------------------------
-- =                                               def tesoroEn
-- girarYDar d (tesoroEn (comprimido m'))
--             ------------------------
-- =                                               HI
-- girarYDar d (tesoroEn m')


-- Lado Der)

-- tesoroEn (Giro d m')
-- --------------------
-- =                                               def tesoroEn
-- girarYDar d (tesoroEn m')


--------------------------------------------------------------------------------------------------------

-- def del lema 

-- para todo x. tesoroEn (compRecto n x) = caminarYDar n (tesoroEn x) ?

-- voy a demostrar por casos:

-- Caso 1: x = (Recto n' m)

-- tesoroEn (compRecto n (Recto n' m)) = caminarYDar n (Recto n' m) ?

-- Lado Izq)

-- tesoroEn (compRecto n (Recto n' m))
--           -----------------------
-- =                                               def compRecto
-- tesoroEn (Recto (n + n') m)
-- --------------------------
-- =                                               def tesoroEn
-- caminarYDar (n + n') (tesoroEn m)
-- --------------------------------
-- =                                               def caminarYDar
-- tesoroEn m

-- Lado Der)

-- caminarYDar n (Recto n' m)
-- --------------------------
-- =                                               def caminarYDar
-- (Recto n' m)


-- Caso 2: x = caso donde no es (Recto n' m)

-- tesoroEn (compRecto n x') = caminarYDar n x' ?

-- LI)

-- tesoroEn (compRecto n x')
--         -----------------
-- =                                               def compRecto
-- tesoroEn (Recto n x')
-- --------------------
-- =                                               def tesoroEn
-- caminarYDar n (tesoroEn x')
-- --------------------------
-- =                                               def caminarYDar
-- tesoroEn x'


-- LD)

-- caminarYDar n x'
-- ---------------
-- =                                               def caminarYDar
-- x'

laMasLarga :: [[a]] -> [a]
laMasLarga [xs] = xs
laMasLarga (xs:xss) = let resto = laMasLarga xss
                        in if length xs > length resto
                        then xs
                        else resto

listas :: [[Int]]
listas = [[1, 2, 3],
          [4, 5],
          [6, 7, 8, 9],
          [10],
          [],
          [11, 12, 13, 14, 15]]

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

losAntecedentesDe :: a -> Tree a -> [a]
losAntecedentesDe x EmptyT          = error "No hay elemento para buscar"
losAntecedentesDe x (NodeT y ti td) = losAntecedentesDeB x (NodeT y ti td)

