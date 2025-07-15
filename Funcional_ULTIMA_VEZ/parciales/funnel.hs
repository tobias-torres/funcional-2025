type Partition a = ([a], [a]) -- tupla de elementos que cumplen o no un criterio respectivamente

data Criteria a b = C (a -> Bool) (a -> b) (a -> b) -- un criterio que, de cumplirse un predicado, aplica la primer funcion; la segunda en caso contrario

data Funnel a b = Initial (Criteria a b) 
                | Step (Criteria a b) (Funnel a b) {-- una estructura linear no vacia que representa los criterios a utilizar que se aplican desde el ultimo: -}
    -- Ej: Step c3 (Step c2 (Initial c1)) aplica primero c1, luego c2, por ultimo c3

partition :: Criteria a b -> [a] -> (Partition a, [b])
partition (C p f g) [] = (([], []), [])
partition (C p f g) (x:xs) = let ((ss, ns), bs) = partition (C p f g) xs
                                in if p x
                                    then ((x:ss, ns), (f x):bs)
                                    else ((ss, x:ns), (g x):bs)

step :: Criteria a b -> ([b] -> b) -> (Partition a, [b]) -> (Partition a, [b])
step c f ((ss, ns), bs) = let ((ss', ns'), bs') = partition c ss 
                            in ((ss', ns ++ ns'), f bs' : bs)

composeC :: Criteria a b -> Criteria b c -> Criteria a c
composeC (C p f g) (C q h k) = C (\a -> p a && q (f a)) (h . f) (k . g)

-- 1) Definir con RE
-- dado un funnel, una función que "reduce" una lista de resultados, y una lista de tipo [a], retorna la particion de elementos a tras aplicar el funnel
appF :: Funnel a b -> ([b] -> b) -> [a] -> (Partition a, [b])
appF (Initial c) f xs     = let (ps, rs) = partition c xs
                                in (ps, [f rs])
appF (Step c funnel) f xs = step c f (appF funnel f xs)

-- que retorna al funnel donde todos los criterios se reemplazan por sus complementos
complementF :: Funnel a b -> Funnel a b
complementF (Initial c)     = Initial (complementC c)
complementF (Step c funnel) = Step (complementC c) (complementF funnel)

complementC :: (Criteria a b) -> (Criteria a b)
complementC (C p f g) = C (not . p) g f

-- dado un funnel, retorna uno donde los criterios se aplican al reves
reverseF :: Funnel a b -> Funnel a b
reverseF (Initial c)     = Initial c
reverseF (Step c funnel) = appendF (reverseF funnel) (Initial c) -- aca es donde hace la magia de hacerlo al reves.

appendF :: Funnel a b -> Funnel a b -> Funnel a b
appendF (Initial c) fn     = Step c fn
appendF (Step c funnel) fn = Step c (appendF funnel fn)

-- -- dado un funnel y una funcion b -> c, lo retorna mapeando sus funciones de a -> b por a -> c
mapF :: (b -> c) -> Funnel a b -> Funnel a c
mapF f (Initial c)     = Initial (mapping c f)
mapF f (Step c funnel) = Step (mapping c f) (mapF f funnel)

mapping :: Criteria a b -> (b -> c) -> Criteria a c
mapping (C p ft ff) f = C p (f . ft) (f . ff)

-- zippea dos funnels, combinando los criteria
zipF :: Funnel a b -> Funnel b c -> Funnel a c
zipF (Initial c) (Initial c')    = Initial (composeC c c') 
zipF (Initial c) (Step c' fun)   = Initial (composeC c c')
zipF (Step c fun) (Initial c')   = Initial (composeC c c')
zipF (Step c fun) (Step c' fun') = Step (composeC c c') (zipF fun fun')


-- 2) Demostrar que:
por ppio de extensionalidad
¿para todo fn. para todo f. para todo xs. appF fn f xs = appF (complementF (complementF fn)) f xs?
¿para todo fn. para todo f. para todo xs. para todo partition. appF fn f xs = appF (complementF (complementF fn)) f xs?
=           lema app f

Lema: 
para todo fn. fn = complementF (complementF fn) ?
sea funnel de tipo Funnel a b. Por ppio de induccion sobre la estructura de funnel, es equivalente demostrar que:

CASO BASE: fn = Initial c)

Initial c = complementF (complementF (Initial c)) ?

Caso Inductivo: fn = Step c fun')

HI) fun' = complementF (complementF fun') !
TI) (Step c fun') = complementF (complementF (Step c fun')) ?

CASO BASE:

LD)

complementF (complementF (Initial c))
            ------------------------
=               def complementF
complementF (Initial (complementC c))
-------------------------------------
=               def complementF
Initial (complementC(complementC c))
        ---------------------------
=               complementC-lema
Initial c


CASO INDUCTIVO:

LI)

(Step c fun')


LD)

complementF (complementF (Step c fun'))
            --------------------------
=               def complementF
complementF (Step (complementC c) (complementF fun'))
-----------------------------------------------------
=               def complementF
Step (complementC (complementC c)) (complementF(complementF fun'))
                                    ----------------------------
=               HI
Step (complementC (complementC c)) fun'
     -----------------------------
=               complementC-lema
Step c fun'

Lema: complementC-lema
por ppio de extensionalidad.
para todo c. c = complementC (complementC c) ?
sea c' de tipo Criteria a b. Se demostrara por casos en la estructura de c'

caso 1: c = (C p ft ff)

(C p ft ff) = complementC (complementC (C p ft ff))

LI)

(C p ft ff)

LD)

complementC (complementC (C p ft ff))
            -------------------------
=               complementC
complementC (C (not . p) ff ft)
-------------------------------
=               complementC
(C p ft ff)


complementC :: (Criteria a b) -> (Criteria a b)
complementC (C p f g) = C (not . p) g f

-- 3) Implementar foldF y recF, en base a la estructura de Funnel a b

foldF :: (Criteria a b -> c) -> (Criteria a b -> c -> c) -> Funnel a b -> c
foldF fi fs (Initial c)  = fi c
foldF fi fs (Step c fun) = fs c (foldF fi fs fun)

recF :: (Criteria a b -> c) -> (Criteria a b -> Funnel a b -> c -> c) -> Funnel a b -> c
recF fi fs (Initial c)  = fi c 
recF fi fs (Step c fun) = fs c fun (recF fi fs fun)

-- 4) Implementar el punto 1 usando foldF o recF según convenga

-- * TODO: Nota mental, acordate que lo que tenes que tener en cuenta aca es que tenes que mirar el tipo de tu C o B.
-- y tenes que ir viendo que parametros toma, para despues ver que mierda tenes que meter en el fold
-- en el caso de appF' mi fun es de tipo c, y toma un c de tipo "c = ([b] -> b) -> [a] -> (Partition a, [b])"

appF' :: Funnel a b -> ([b] -> b) -> [a] -> (Partition a, [b]) -- c = ([b] -> b) -> [a] -> (Partition a, [b])
appF' = foldF (\c f xs -> let (ps, rs) = partition c xs in (ps, [f rs]))
                (\c h f xs  -> step c f (h f xs))

complementF' :: Funnel a b -> Funnel a b
complementF' = foldF (\c -> Initial (complementC c)) (\c fun -> Step (complementC c) fun)

appendF' :: Funnel a b -> (Funnel a b -> Funnel a b) -- c = (Funnel a b -> Funnel a b) 
appendF' = foldF (\c fun -> Step c fun) (\c h fun -> Step c (h fun) )

zipF' :: Funnel a b -> Funnel b c -> Funnel a c
zipF' = foldF (\c fun -> case fun of 
                        Initial c' -> Initial (composeC c c')
                        Step c' fun -> Initial (composeC c c'))
              (\c h fun -> case fun of
                        Initial c' -> Initial (composeC c c')
                        Step c' fun' -> Step (composeC c c') (h fun'))

