data Value = Vint Int | Vstring String | Vbool Bool | Vlist [Value] deriving (Show)

type Field = String 

data Svson i = Empty | Obj i (Field -> Maybe Value) (Svson i) (Svson i) 

foldS :: a -> (i -> (Field -> Maybe Value) -> a -> a -> a) -> Svson i -> a
foldS fe fo Empty           = fe
foldS fe fo (Obj i f s1 s2) = fo i f (foldS fe fo s1) (foldS fe fo s2) 

belongs :: Ord i => i -> Svson i -> Bool
belongs i = foldS False (\i' f b b' -> if i == i'
                                                then True
                                                else if i < i'
                                                        then b 
                                                        else b')