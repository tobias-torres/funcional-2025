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
belongs e (Obj i f s s') = e == i || belongs e s || belongs e s'

