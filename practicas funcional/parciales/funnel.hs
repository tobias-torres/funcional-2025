type Partition a = ([a], [a])                       -- tupla de elementos que cumplen o no un criterio respectivamente

data Criteria a b = C (a -> Bool) (a -> b) (a -> b) -- un criterio que, de cumplirse un predicado, aplica la primer funcion; la segunda en caso contrario

data Funnel a b = Initial (Criteria a b) -- Estructura lineal no vacia que representa los criterios a utilizar que se aplican desde el ultimo:
                | Step (Criteria a b) (Funnel a b)

partition :: Criteria a b -> [a] -> (Partition a, [b]) -- (([a], [a]), [b]) 
partition (C p f g) []     = (([],[]), [])
partition (C p f g) (x:xs) = let ((ts, fs), ys) = partition (C p f g) xs -- El resultado de la recursion 
                                in if p x -- si se cumple, proceso el primer elemento 
                                    then ((x:ts, fs), f x : ys)
                                    else ((ts, x:fs), g x : ys)

-- Aplica el nuevo criterio a los elementos que no cumplieron criterios anteriores (la parte de la partición que quedó sin procesar), 
-- y luego combina los resultados nuevos con los previos usando la función combinadora.
step :: Criteria a b -> ([b] -> b) -> (Partition a, [b]) -> (Partition a, [b])
step c f ((ts, fs), ys) = let ((ts', fs'), ys') = partition c fs -- los elementos de fs procesados con el nuevo criterio 
                            in ((ts ++ ts', fs'), f ys' : ys)

-- Para un elemento a, primero evalúa el primer criterio y luego, sobre el resultado b obtenido, 
-- aplica el segundo criterio para finalmente obtener c.
composeC :: Criteria a b -> Criteria b c -> Criteria a c
composeC (C p1 f1 g1) (C p2 f2 g2) = C (\x -> p1 x && p2 (f1 x)) (f2 . f1) (g2 . g1)

-- La idea para generar la particion es aplicar sucesivamente los dif criterias encontrados en un funnel, una lista dada que cumplen todos los predicados vistos.
-- A la vez que se recolectan resultados de transformaciones para cada elemento, tanto los que van a la parte que cumplen el criterio, como las que no lo cumplen.

-- 1) Definir con RE
-- dado un funnel, una función que "reduce" una lista de resultados, y una lista de tipo [a], retorna la particion de elementos a tras aplicar el funnel
appF :: Funnel a b -> ([b] -> b) -> [a] -> (Partition a, [b])
appF (Initial c) f xs     = let (ts, fs) = partition c xs
                                in (ts, [f fs])
appF (Step c funnel) f xs = step c f (appF funnel f xs)

-- que retorna al funnel donde todos los criterios se reemplazan por sus complementos
complementF :: Funnel a b -> Funnel a b
complementF (Initial c) = Initial (complementC c)
complementF (Step c f)  = Step (complementC c) (complementF f)

complementC :: Criteria a b -> Criteria a b
complementC (C p f g) = C (not . p) g f

-- dado un funnel, retorna uno donde los criterios se aplican al revez
reverseF :: Funnel a b -> Funnel a b
reverseF (Initial c)     = (Initial c)
reverseF (Step c funnel) = append (reverseF funnel) (Initial c) 

append :: Funnel a b -> Funnel a b -> Funnel a b
append (Initial c) fun   = Step c fun
append (Step c fun) fun' = Step c (append fun fun')

-- -- dado un funnel y una funcion b -> c, lo retorna mapeando sus funciones de a -> b por a -> c
mapF :: (b -> c) -> Funnel a b -> Funnel a c
mapF f (Initial c)  = (Initial (mapC f c))
mapF f (Step c fun) = Step (mapC f c) (mapF f fun)

mapC :: (b -> c) -> (Criteria a b) -> (Criteria a c)
mapC f (C p f' g) = C p (f . f') (f . g)

-- zippea dos funnels, combinando los criteria
zipF :: Funnel a b -> Funnel b c -> Funnel a c
zipF (Initial c) (Initial c')    = Initial (composeC c c')
zipF (Initial c) (Step c' fun)   = Initial (composeC c c')
zipF (Step c fun) (Initial c')   = Initial (composeC c c')
zipF (Step c fun) (Step c' fun') = Step (composeC c c') (zipF fun fun')

foldF :: (Criteria a b -> c) -> (Criteria a b -> c -> c) -> (Funnel a b) -> c
foldF fi fs (Initial c)  = fi c
foldF fi fs (Step c fun) = fs c (foldF fi fs fun)

recF :: (Criteria a b -> c) -> (Criteria a b -> Funnel a b -> c -> c) -> (Funnel a b) -> c
recF fi fs (Initial c)  = fi c
recF fi fs (Step c fun) = fs c fun (recF fi fs fun)

appF' :: Funnel a b -> ([b] -> b) -> [a] -> (Partition a, [b])
appF' = foldF (\c f xs -> let (ts, fs) = partition c xs
                                in (ts, [f fs]))
              (\c fun f xs -> step c f (fun f xs))

complementF' :: Funnel a b -> Funnel a b
complementF' = foldF (\c -> Initial (complementC c))
                     (\c fun -> Step (complementC c) fun)

reverseF' :: Funnel a b -> Funnel a b
reverseF' = foldF Initial
                  (\c fun -> append fun (Initial c))

mapF' :: (b -> c) -> Funnel a b -> Funnel a c
mapF' = flip (foldF (\c f -> (Initial) (mapC f c))
                    (\c fun f -> Step (mapC f c) (fun f)))

zipF' :: Funnel a b -> Funnel b c -> Funnel a c
zipF' = foldF (\c fun -> case fun of
                            (Initial c')   -> Initial (composeC c c')
                            (Step c' fun') -> Initial (composeC c c'))
              (\c fun fun' -> case fun' of
                                (Initial c') -> Initial (composeC c c')
                                (Step c' funnel) -> Step (composeC c c') (fun funnel))

-- zippea dos funnels, combinando los criteria
-- zipF :: Funnel a b -> Funnel b c -> Funnel a c
-- zipF (Initial c) (Initial c')    = Initial (composeC c c')
-- zipF (Initial c) (Step c' fun)   = Initial (composeC c c')
-- zipF (Step c fun) (Initial c')   = Initial (composeC c c')
-- zipF (Step c fun) (Step c' fun') = Step (composeC c c') (zipF fun fun')