data Accion a = Paso a | SaltoArriba a | SaltoAdelante a | Girar a

type Tiempo = Int -- el instante en el que sucede un movimiento

type Duracion = Int -- cantidad de tiempos que dura el movimiento

data Animacion a = Espera Duracion -- durante la duración dada no hay acciones
                | Mov Duracion (Accion a) -- un cierto movimiento con una duración dada
                | Sec (Animacion a) (Animacion a) -- secuencia (la 2da empieza al terminar la 1era)
                | Par (Animacion a) (Animacion a) -- paralelo (arrancan juntas y dura lo que la más larga)

type Frame a = [Accion a] -- acciones simultáneas en un tiempo específico

type Simulador a = Tiempo -> Frame a

-- 1)
combinarSinDuplicados :: [Int] -> [Int] -> [Int]
combinarSinDuplicados [] ys        = ys
combinarSinDuplicados xs []        = xs
combinarSinDuplicados (x:xs)(y:ys) = if x == y
                                        then x : combinarSinDuplicados xs ys
                                        else if x > y
                                                then y : combinarSinDuplicados (x:xs) ys
                                                else x : combinarSinDuplicados xs (y:ys)

-- 2.a
duracion :: Animacion a -> Int
duracion (Espera d)  = d
duracion (Mov d act) = d
duracion (Sec a a')  = duracion a + duracion a'
duracion (Par a a')  = max (duracion a) (duracion a')

-- 2.b
alargar :: Int -> Animacion a -> Animacion a
alargar n (Espera d)  = Espera (n * d)
alargar n (Mov d act) = Mov (n * d) act
alargar n (Sec a a')  = Sec (alargar n a) (alargar n a')
alargar n (Par a a')  = Par (alargar n a) (alargar n a')

-- 2.c
simular :: Animacion a -> [Frame a]
simular (Espera d)  = replicate' d []
simular (Mov d act) = replicate' d [act]
simular (Sec a a')  = simular a ++ simular a'
simular (Par a a')  = mergeListas (simular a) (simular a')

mergeListas :: [[a]] -> [[a]] -> [[a]]
mergeListas [] yss         = yss
mergeListas xss []         = xss
mergeListas (xs:xss) (ys:yss) = (xs ++ ys) : mergeListas xss yss 

replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x : replicate' (n-1) x

-- 2.d
tiemposDeEspera :: Animacion a -> [Tiempo]
tiemposDeEspera (Espera d)  = contarHasta d
tiemposDeEspera (Mov d act) = []
tiemposDeEspera (Sec a a')  = tiemposDeEspera a ++ map (+ duracion a) (tiemposDeEspera a')
tiemposDeEspera (Par a a')  = combinarSinDuplicados (tiemposDeEspera a) (tiemposDeEspera a')

contarHasta :: Int -> [Int]
contarHasta 0 = []
contarHasta n = contarHasta (n - 1) ++ [n]

-- 4
foldA :: (Int -> b) -> (Duracion -> Accion a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Animacion a -> b
foldA fe fm fs fp (Espera d)  = fe d
foldA fe fm fs fp (Mov d act) = fm d act
foldA fe fm fs fp (Sec a a')  = fs (foldA fe fm fs fp a) (foldA fe fm fs fp a')
foldA fe fm fs fp (Par a a')  = fp (foldA fe fm fs fp a) (foldA fe fm fs fp a')

recA :: (Int -> b) -> (Duracion -> Accion a -> b) -> (Animacion a -> Animacion a -> b -> b -> b) -> (Animacion a -> Animacion a -> b -> b -> b) -> Animacion a -> b
recA fe fm fs fp (Espera d)  = fe d
recA fe fm fs fp (Mov d act) = fm d act
recA fe fm fs fp (Sec a a')  = fs a a' (recA fe fm fs fp a) (recA fe fm fs fp a')
recA fe fm fs fp (Par a a')  = fp a a' (recA fe fm fs fp a) (recA fe fm fs fp a')

-- 5

duracion' :: Animacion a -> Int
duracion' = foldA id const (+) max

alargar' :: Int -> Animacion a -> Animacion a
alargar' n = foldA (\d -> Espera (n * d)) (\d act -> Mov (n * d) act) (\a a' -> Sec a a') (\a a' -> Par a a')
-- alargar' = foldA (Espera . (*)) (Mov . (*)) Sec Par

simular' :: Animacion a -> [Frame a]
simular' = foldA (\d -> replicate' d []) (\d act -> replicate' d [act]) (\a a' -> a ++ a') (\a a' -> mergeListas' a a')

mergeListas' :: [[a]] -> [[a]] -> [[a]]
mergeListas' = foldr (\xs xss yss -> case yss of 
                                       [] -> xss yss
                                       (ys:yss') -> (xs ++ ys) : xss yss') id

tiemposDeEspera' :: Animacion a -> [Tiempo]
tiemposDeEspera' = recA (\d -> contarHasta d) 
                        (\d act -> [])
                        (\a a' t t' -> t ++ map (+ duracion a) t')
                        (\a a' t t'-> combinarSinDuplicados t t')

-- 3 Demostracion
para todo k >= 0. duracion . (alargar k) = (k*) . duracion
por ppio de extensionalidad
para todo k >= 0. para todo anim. (duracion . (alargar k)) anim = ((k*) . duracion) anim ?
por def de (.) dos veces.
para todo k >= 0. para todo anim. duracion (alargar k anim) = k * (duracion anim) ?
sea k' de tipo Int y anim' de tipo Animacion a, por ppio de induccion sobre la estructura de anim', es equivalente demostrar que:

CASO BASE 1: anim' = Espera d)

duracion (alargar k (Espera d)) = k * (duracion (Espera d)) ?

CASO BASE 2: anim' = Mov d act)

duracion (alargar k (Mov d act)) = k * (duracion (Mov d act)) ?

CASO INDUCTIVO 1: anim' = Sec a a')

HI1) duracion (alargar k a) = k * (duracion a) !
HI2) duracion (alargar k a') = k * (duracion a') !
TI) duracion (alargar k (Sec a a')) = k * (duracion (Sec a a')) ?

CASO INDUCTIVO 2: anim' = Par a a')

HI1) duracion (alargar k a) = k * (duracion a) !
HI2) duracion (alargar k a') = k * (duracion a') !
TI) duracion (alargar k (Par a a')) = k * (duracion (Par a a')) ?

CASO BASE 1:

LI)

duracion (alargar k (Espera d))
        ----------------------
=                               def alargar
duracion (Espera (k * d))
-------------------------
=                               def duracion
(k * d)


LD)

k * (duracion (Espera d))
    ---------------------
=                               def duracion
k * d


CASO BASE 2:

LI)

duracion (alargar k (Mov d act))
         ----------------------
=                               def alargar
duracion (Mov (k * d) act)
--------------------------
=                               def duracion
(k * d)


LD)

k * (duracion (Mov d act))
    ---------------------
=                               def duracion
k * d

CASO INDUCTIVO 1:

LI)

duracion (alargar k (Sec a a'))
         ----------------------
=                               def alargar
duracion (Sec (alargar k a) (alargar k a'))
-------------------------------------------
=                               def duracion
duracion(alargar k a) + duracion(alargar k a')
---------------------   ----------------------
=                               H1, H2
k * (duracion a) + k * (duracion a')
-------------------------------------
=                               factor comun
k * (duracion a + duracion a')


LD)

k * (duracion (Sec a a'))
    ---------------------
=                               def duracion
k * (duracion a + duracion a')

CASO INDUCTIVO 2:

LI)

duracion (alargar k (Par a a'))
         ---------------------
=                               def alargar
duracion (Par (alargar k a) (alargar k a'))
-------------------------------------------
=                               def duracion
max (duracion(alargar k a)) (duracion(alargar k a'))
    ----------------------  ------------------------
=                               HI1, HI2
max (k * (duracion a)) (k * (duracion a'))
------------------------------------------
=                               def max
if (k * (duracion a)) >= (k * (duracion a')) then (k * (duracion a)) else (k * (duracion a'))
---------------------------------------------------------------------------------------------
=                               lema if-else
k * (if duracion a >= duracion a' then duracion a else duracion a')

LD)

k * (duracion (Par a a'))
    ---------------------
=                               def duracion
k * max (duracion a) (duracion a')
    ------------------------------
=                               def max
k * (if (duracion a) >= (duracion a') then (duracion a) else (duracion a'))


duracion :: Animacion a -> Int
duracion (Espera d)  = d
duracion (Mov d act) = d
duracion (Sec a a')  = duracion a + duracion a'
duracion (Par a a')  = max (duracion a) (duracion a')

-- 2.b
alargar :: Int -> Animacion a -> Animacion a
alargar n (Espera d)  = Espera (n * d)
alargar n (Mov d act) = Mov (n * d) act
alargar n (Sec a a')  = Sec (alargar n a) (alargar n a')
alargar n (Par a a')  = Par (alargar n a) (alargar n a')