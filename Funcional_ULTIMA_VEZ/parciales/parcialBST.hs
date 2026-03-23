data Value = Vint Int | Vstring String | Vbool Bool | Vlist [Value] deriving Show

type Field = String

data Svson i = Empty | Obj i (Field -> Maybe Value) (Svson i) (Svson i)

-- La estructura Svson i representa un árbol binario de búsqueda (BST) donde cada nodo contiene:
-- Un índice de tipo i (que debe ser ordenable),
-- Una función que mapea nombres de campos a valores opcionales,
-- Dos subárboles izquierdo y derecho

valuesOf :: [Field] -> (Field -> Maybe Value) -> [Value]
valuesOf = undefined
-- Obtiene los valores de una lista de campos de un objeto

valuesWithFields :: [Field] -> (Field -> Maybe Value) -> [(Field, Value)]
valuesWithFields = undefined
-- Obtiene pares (campo, valor) para los campos que existen

only :: [Field] -> (Field -> Maybe Value) -> (Field -> Maybe Value)
only = undefined
-- Restringe un objeto a solo los campos especificados

update :: Field -> Value -> (Field -> Maybe Value) -> (Field -> Maybe Value)
update = undefined
-- Actualiza o agrega un campo con un valor en un objeto

singleton :: Field -> Value -> (Field -> Maybe Value)
singleton = undefined
-- Crea un objeto con un solo campo y valor

-- EJERCICIOS 1

-- Devuelve una lista con todos los índices presentes en la estructura, en cualquier orden.
indices :: Svson i -> [i]
indices Empty           = []
indices (Obj i f s1 s2) = i : indices s1 ++ indices s2

indices' :: Svson i -> [i]
indices' = foldS [] (\i f s1 s2 -> i : s1 ++ s2)

-- Indica si un índice dado existe en la estructura. Debe aprovechar la propiedad de BST para ser eficiente.
belongs :: Ord i => i -> Svson i -> Bool
belongs e Empty           = False
belongs e (Obj i f s1 s2) = e == i || e < i && belongs e s1 || belongs e s2

belongs' :: Ord i => i -> Svson i -> Bool
belongs' = flip (foldS (const False) (\i f s1 s2 e -> e == i || e < i && s1 e|| s2 e))

-- Busca un índice específico y devuelve los valores de los campos solicitados para ese índice. Si el índice no existe, 
-- devuelve una lista vacía.
lookupProjecting :: Ord i => i -> [Field] -> Svson i -> [Value]
lookupProjecting i xs Empty            = []
lookupProjecting i xs (Obj i' f s1 s2) = if i == i' 
                                           then valuesOf xs f 
                                           else if i < i' 
                                                then lookupProjecting i xs s1 
                                                else lookupProjecting i xs s2

lookupProjecting' :: Ord i => i -> [Field] -> Svson i -> [Value]
lookupProjecting' i xs = foldS [] (\i' f s1 s2 -> if i == i'
                                                   then valuesOf xs f
                                                   else if i < i'
                                                        then s1 
                                                        else s2)

-- Actualiza o inserta un campo en el objeto con el índice dado. Si el índice no existe, crea un nuevo nodo en la posición correcta del BST.
upsert :: Ord i => i -> Field -> Value -> Svson i -> Svson i
upsert i field v Empty            = Obj i (singleton field v) Empty Empty
upsert i field v (Obj i' f s1 s2) = if i == i'
                                  then Obj i (update field v f) s1 s2
                                  else if i < i'
                                        then Obj i' f (upsert i field v s1) s2
                                        else Obj i' f s1 (upsert i field v s2)

upsert' :: Ord i => i -> Field -> Value -> Svson i -> Svson i
upsert' i field v = recSvson (Obj i (singleton field v) Empty Empty) 
                          (\i' f svson1 svson2 s1 s2 -> if i == i'
                                           then Obj i (update field v f) s1 s2
                                           else if i< i'
                                                then Obj i' f svson1 s2
                                                else Obj i' f s1 svson2)


foldS :: a -> (i -> (Field -> Maybe Value) -> a -> a -> a) -> Svson i -> a
foldS fe fo Empty           = fe
foldS fe fo (Obj i f s1 s2) = fo i f (foldS fe fo s1) (foldS fe fo s2) 

recSvson :: a -> (i -> (Field -> Maybe Value) -> Svson i -> Svson i -> a -> a -> a) -> Svson i -> a
recSvson fe fo Empty           = fe
recSvson fe fo (Obj i f s1 s2) = fo i f s1 s2 (recSvson fe fo s1) (recSvson fe fo s2)


-- Realiza la intersección entre dos estructuras, combinando los objetos que tienen índices en común.
mkObj :: Ord i => Svson i -> Svson i -> Svson i
mkObj s1 s2 = toSvson (intersect (toList s1) (toList s2))

intersect :: Eq i => [(i, Field -> Maybe Value)] -> [(i, Field -> Maybe Value)] -> [(i, Field -> Maybe Value)] 
intersect = foldr (\(i,fv) g -> \ifvs' -> if null ifvs' 
                                            then []
                                            else if elemI i ifvs' 
                                                then (i,fv) : g ifvs' 
                                                else g ifvs')
                    (const [])

elemI :: Eq i => i -> [(i, Field -> Maybe Value)] -> Bool 
elemI = flip (foldr (\(i, fv) g -> \i' -> i' == i || g i')
                    (const False))

toList :: Svson i -> [(i, Field -> Maybe Value)]
toList = foldS [] (\i fv ifvs1 ifvs2 -> (i, fv) : ifvs1 ++ ifvs2) 

toSvson :: Ord i => [(i, Field -> Maybe Value)] -> Svson i 
toSvson = foldr (\(i,fv) s -> insert i fv s)  
                Empty
-- toSvson = foldr (uncurry insert) Empty


insert :: Ord i => i -> (Field -> Maybe Value) -> Svson i -> Svson i 
insert i fv = recSvson (Obj i fv Empty Empty)
                    (\j f sr1 s1 sr2 s2 -> if i == j 
                                            then Obj j f s1 s2 
                                            else if i < j 
                                                    then Obj j f sr1 s2 
                                                    else Obj j f s1 sr2) 

-- Devuelve hasta n objetos que cumplan la condición dada, junto con el número real de objetos encontrados. 
-- Debe priorizar los índices menores (recorrido in-order del BST).
-- takeUpSatisfying :: Ord i => Int -> (i -> Bool) -> Svson i -> (Svson i, Int)
-- takeUpSatisfying 


-- EJERCICIO 2
-- Definir foldSvson y recSvson 



-- EJERCICIO 4
-- para todo i. belongs i = elem i . indices 

esPar :: Int -> Bool
esPar n = n `mod` 2 == 0



-- EJEMPLO 
ejemplo :: Svson Int
ejemplo = 
  Obj 15 (singleton "x" (Vint 15))
    (Obj 13 (singleton "x" (Vint 13))
      (Obj 10 (singleton "x" (Vint 10))
        (Obj 5 (singleton "x" (Vint 5))
          (Obj 3 (singleton "x" (Vint 3)) Empty Empty)
          (Obj 6 (singleton "x" (Vint 6)) Empty Empty))
        (Obj 12 (singleton "x" (Vint 12)) Empty Empty))
      (Obj 14 (singleton "x" (Vint 14)) Empty Empty))
    (Obj 20 (singleton "x" (Vint 20)) Empty Empty)

ejemplo2 :: Svson Int
ejemplo2 = Obj 15 (singleton "x" (Vint 15)) Empty (Obj 20 (singleton "x" (Vint 20)) Empty Empty)


instance Show i => Show (Svson i) where
  show Empty = "Empty"
  show (Obj i _ l r) =
    "Obj " ++ show i ++ " <func> (" ++ show l ++ ") (" ++ show r ++ ")"