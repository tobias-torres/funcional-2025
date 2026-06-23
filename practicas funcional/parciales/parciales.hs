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

indices :: Svson i -> [i]
indices Empty          = []
indices (Obj i f s s') = i : indices s ++ indices s'

belongs :: Ord i => i -> Svson i -> Bool
belongs e Empty          = False
belongs e (Obj i f s s') = if e == i then True else if e < i then belongs e s else belongs e s'

lookupProjecting :: Ord i => i -> [Field] -> Svson i -> [Value]
lookupProjecting indiceABuscar xs Empty          = []
lookupProjecting indiceABuscar xs (Obj i f s s') = if indiceABuscar == i
                                                    then valuesOf xs f
                                                    else if indiceABuscar < i
                                                        then lookupProjecting indiceABuscar xs s
                                                        else lookupProjecting indiceABuscar xs s'

upsert :: Ord i => i -> Field -> Value -> Svson i -> Svson i
upsert indiceAInsertar campo valor Empty          = 
upsert indiceAInsertar campo valor (Obj i f s s') = if indiceAInsertar == i
                                                     then (Obj i (update campo valor ))


valuesOf :: [Field] -> (Field -> Maybe Value) -> [Value]
-- Obtiene los valores de una lista de campos de un objeto

valuesWithFields :: [Field] -> (Field -> Maybe Value) -> [(Field, Value)]
-- Obtiene pares (campo, valor) para los campos que existen

only :: [Field] -> (Field -> Maybe Value) -> (Field -> Maybe Value)
-- Restringe un objeto a solo los campos especificados

update :: Field -> Value -> (Field -> Maybe Value) -> (Field -> Maybe Value)
-- Actualiza o agrega un campo con un valor en un objeto

singleton :: Field -> Value -> (Field -> Maybe Value)
-- Crea un objeto con un solo campo y valor
```