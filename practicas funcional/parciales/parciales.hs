data Value = Vint Int | Vstring String | Vbool Bool | Vlist [Value]

type Field = String

data Svson i = Empty | Obj i (Field -> Maybe Value) (Svson i) (Svson i)

datosCerati :: Field -> Maybe Value
datosCerati "nombre"    = Just (Vstring "Gustavo Cerati")
datosCerati "banda"     = Just (Vstring "Soda Stereo")
datosCerati "solista"   = Just (Vbool True)
datosCerati "discos"    = Just (Vlist [Vstring "Bocanada", Vstring "Fuerza Natural"])
datosCerati _           = Nothing 

datosZeta :: Field -> Maybe Value
datosZeta "nombre"      = Just (Vstring "Zeta Bosio")
datosZeta "instrumento" = Just (Vstring "Bajo")
datosZeta _             = Nothing

datosCharly :: Field -> Maybe Value
datosCharly "nombre"      = Just (Vstring "Charly Alberti")
datosCharly "instrumento" = Just (Vstring "Batería")
datosCharly _             = Nothing

svson = Obj 1 datosCerati 
            (Obj 2 datosZeta Empty Empty)
            (Obj 3 datosCharly Empty Empty)

-- 1. Obtiene los valores de una lista de campos, ignorando los que no existen
valuesOf :: [Field] -> (Field -> Maybe Value) -> [Value]
valuesOf [] dict = []
valuesOf (f:fs) dict = case dict f of
    Just v  -> v : valuesOf fs dict
    Nothing -> valuesOf fs dict

-- 2. Igual que la anterior, pero guarda la tupla (campo, valor)
valuesWithFields :: [Field] -> (Field -> Maybe Value) -> [(Field, Value)]
valuesWithFields [] dict = []
valuesWithFields (f:fs) dict = case dict f of
    Just v  -> (f, v) : valuesWithFields fs dict
    Nothing -> valuesWithFields fs dict

-- 3. Crea un diccionario nuevo que "bloquea" el acceso a campos no permitidos
only :: [Field] -> (Field -> Maybe Value) -> (Field -> Maybe Value)
only camposPermitidos dict = \f -> if elem f camposPermitidos 
                                   then dict f 
                                   else Nothing

-- 4. Crea un diccionario nuevo que intercepta el campo a actualizar, o delega en el viejo
update :: Field -> Value -> (Field -> Maybe Value) -> (Field -> Maybe Value)
update campoNuevo valorNuevo dictViejo = \f -> if f == campoNuevo 
                                               then Just valorNuevo 
                                               else dictViejo f

-- 5. El diccionario más básico de todos: solo conoce un campo
singleton :: Field -> Value -> (Field -> Maybe Value)
singleton campo valor = \f -> if f == campo 
                              then Just valor 
                              else Nothing

indices :: Svson i -> [i]
indices Empty          = []
indices (Obj i f s s') = i : indices s ++ indices s'

belongs :: Ord i => i -> Svson i -> Bool
belongs _ Empty            = False
belongs i (Obj i' _ s1 s2) = i == i' || i < i' && belongs i s1 || belongs i s2

elem :: Eq a => a -> [a] -> Bool
elem e []     = False
elem e (x:xs) = e == x || elem e xs

lookupProjecting :: Ord i => i -> [Field] -> Svson i -> [Value]
lookupProjecting indiceABuscar xs Empty          = []
lookupProjecting indiceABuscar xs (Obj i f s s') = if indiceABuscar == i
                                                    then valuesOf xs f
                                                    else if indiceABuscar < i
                                                        then lookupProjecting indiceABuscar xs s
                                                        else lookupProjecting indiceABuscar xs s'

upsert :: Ord i => i -> Field -> Value -> Svson i -> Svson i
upsert indiceAInsertar field value Empty          = Obj indiceAInsertar (singleton field value) Empty Empty
upsert indiceAInsertar field value (Obj i f s s') = if indiceAInsertar == i
                                                     then (Obj i (update field value f) s s')
                                                     else if indiceAInsertar < i
                                                        then Obj i f (upsert indiceAInsertar field value s) s'
                                                        else Obj i f s (upsert indiceAInsertar field value s')

-- mkObj :: Svson i -> Svson i -> Svson i
-- mkObj s s' = 

insert :: Ord i => i -> (Field -> Maybe Value) -> Svson i -> Svson i
insert i fv Empty              = Obj i fv Empty Empty
insert i fv (Obj i' fv' s1 s2) = if i < i' then Obj i' fv' (insert i fv s1) s2
                                           else Obj i' fv' s1 (insert i fv s2)

-- Que devuelve hasta `n` objetos que cumplan la condición dada, junto con el número real 
-- de objetos encontrados. Debe priorizar los índices menores (recorrido in-order del BST).
takeUpSatisfying :: Ord i => Int -> (i -> Bool) -> Svson i -> (Svson i, Int)
takeUpSatisfying 0 _ _                = (Empty,0)
takeUpSatisfying _ _ Empty            = (Empty,0)
takeUpSatisfying n f (Obj i fv s1 s2) = case s1 of
                                         Empty -> if f i then let (s,m) = takeUpSatisfying (n-1) f s2
                                                               in (insert i fv s, m+1)
                                                         else takeUpSatisfying n f s2
                                         _     -> let (s,m) = takeUpSatisfying n f s1
                                                   in if n > m then if f i then if n > m+1 then let (s',m') = takeUpSatisfying (n-m-1) f s2
                                                                                                 in (insert i fv s', m'+1)
                                                                                           else (insert i fv s, m+1)
                                                                           else takeUpSatisfying (n-m) f s2
                                                               else (s,m)


foldS :: a -> (i -> (Field -> Maybe Value) -> a -> a -> a) -> Svson i -> a
foldS fe fo Empty          = fe
foldS fe fo (Obj i f s s') = fo i f (foldS fe fo s) (foldS fe fo s')

recS :: a -> (i -> (Field -> Maybe Value) -> Svson i -> a -> Svson i -> a -> a) -> Svson i -> a
recS fe fo Empty          = fe
recS fe fo (Obj i f s s') = fo i f s (recS fe fo s) s' (recS fe fo s')


indices' :: Svson i -> [i]
indices' = foldS [] (\i f xs ys -> i : xs ++ ys)

belongs' :: Ord i => i -> Svson i -> Bool
belongs' e = foldS False (\i' f b b' -> if e == i' 
                                            then True
                                            else if e < i'
                                                then b
                                                else b')

-- belongs' = flip (foldS (const False) (\i' f b b' e -> if e == i' 
--                                             then True
--                                             else if e < i'
--                                                 then b e
--                                                 else b' e))

lookupProjecting' :: Ord i => i -> [Field] -> Svson i -> [Value]
lookupProjecting' e fxs = foldS [] (\i f xs ys -> if e == i
                                                    then valuesOf fxs f
                                                    else if e < i
                                                            then xs 
                                                            else ys )

-- lookupProjecting' e = flip (foldS (const []) (\i f xs ys fxs -> if e == i
--                                                     then valuesOf fxs f
--                                                     else if e < i
--                                                             then xs fxs
--                                                             else ys fxs))

upsert' :: Ord i => i -> Field -> Value -> Svson i -> Svson i
upsert' indiceAInsertar f v = recS (Obj indiceAInsertar (singleton f v) Empty Empty)
                                   (\i g s r s' r' -> if indiceAInsertar == i
                                                        then (Obj i (update f v g) s s')
                                                        else if indiceAInsertar < i
                                                            then Obj i g r s'
                                                            else Obj i g s r')


-- DEMO:
belongs i s = elem i (indices s) ?
sea i' de tipo `i` y un s' de tipo Svson, por ppio de induccion sobre la estructura de s, es equivalente demostrar que:

Caso Base: s' = Empty

belongs i' Empty = elem i' (indices Empty) ?

Caso inductivo: s' = (Obj i fv s1 s2)

HI1) belongs i' s1 = elem i' (indices s1) ! 
HI2) belongs i' s2 = elem i' (indices s2) !
TI) belongs i' (Obj i fv s1 s2) = elem i' (indices (Obj i fv s1 s2)) !

Caso Base:

I)

belongs i' Empty
----------------
=                   def de belongs
False

D)

elem i' (indices Empty)
        --------------
=                   def de indices
elem i' []
----------
=                   def de elem
False

Caso Inductivo:

I)

belongs i' (Obj i fv s1 s2)
---------------------------
=                   def de belongs
i' == i || i < i' && belongs i s1 || belongs i s2
                     ------------    ------------
i' == i || i < i' && elem i' (indices s1) || elem i' (indices s2)

D)

elem i' (indices (Obj i fv s1 s2))
        -------------------------
=                   def de indices
elem i' (i : indices s1 ++ indices s2)
--------------------------------------
=                   def de elem
i' == i || elem i' (indices s1 ++ indices s2)
           ----------------------------------
=                   lema elem-distr
i' == i || i < i' && elem i' (indices s1) || elem i' (indices s2)                


-- LEMA: elem-distr
-- por ppio de extensionalidad
-- para todo xs. para todo ys. elem x xs || elem x ys = elem x (xs ++ ys)
-- sean ms y ns dos listas finitas, por ppio de induccion sobre la estructura de ms, es equivalente demostrar que:

-- Caso base: ms = []

-- elem x [] || elem x ns = elem x ([] ++ ns)?

-- Caso inductivo: ms = (m : ms')

-- HI) elem x ms' || elem x ns = elem x (ms ++ ns) !
-- TI) elem x (m: ms') || elem x ns = elem x ((m: ms') ++ ns) ?

-- Caso Base:

-- LI)

-- elem x [] || elem x ns
-- ---------
-- =                       def elem
-- False || elem x ns
-- ------------------
-- =                       def de neutro
-- elem x ns

-- LD)

-- elem x ([] ++ ns)
--         --------
-- =                       def ++
-- elem x ns

-- Caso Inductivo:

-- LI)

-- elem x (m: ms') || elem x ns 
-- ---------------
-- =                       def elem
-- x == m || elem x ms' || elem x ns
--         -------------------------
-- =                       HI
-- x == m || elem x (ms ++ ns)

-- LD)

-- elem x ((m: ms') ++ ns)
--         --------------
-- =                       def ++
-- elem x (m : ( ms' ++ ns))
-- -------------------------
-- =                       def elem
-- x == m || elem x (ms' ++ ns)