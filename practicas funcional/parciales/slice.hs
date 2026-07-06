data SliceExp a = Base [a] 
                    | Take Int (SliceExp a)
                    | Drop Int (SliceExp a) deriving (Show)

ej = Take 3 (Take 2 (Drop 3 (Drop 1 (Base [1..10])))) -- [4,5]

ej2 = Take 1 (Drop 1 (Base [1,2,3]))

materialize :: SliceExp a -> [a]
materialize (Base xs)  = xs
materialize (Take n s) = take n (materialize s)
materialize (Drop n s) = drop n (materialize s)

lenS :: SliceExp a -> Int
lenS (Base xs)  = length xs
lenS (Take n s) = min n (lenS s)
lenS (Drop n s) = max 0 (lenS s - n)

normalize :: SliceExp a -> SliceExp a
normalize (Base xs)  = (Base xs)
normalize (Take n s) = normalizarTake n (normalize s)
normalize (Drop n s) = normalizarDrop n (normalize s)

normalizarTake :: Int -> SliceExp a -> SliceExp a
normalizarTake n (Take n' s) = (Take (n+n') s)
normalizarTake n s           = Take n s

normalizarDrop :: Int -> SliceExp a -> SliceExp a
normalizarDrop n (Drop n' s) = Drop (n+n') s
normalizarDrop n s           = Drop n s

takeS :: Int -> SliceExp a -> SliceExp a
takeS n (Base xs)  = Base (take n xs)
takeS n (Take m s) = Take m (takeS (min n m) s)
takeS n (Drop m s) = Drop m (takeS (n+m) s) 

-- esquema primitivo y recursivo

foldS :: ([a] -> b) -> (Int -> b -> b) -> (Int -> b -> b) -> SliceExp a -> b
foldS fb ft fd (Base xs)  = fb xs
foldS fb ft fd (Take n s) = ft n (foldS fb ft fd s)
foldS fb ft fd (Drop n s) = fd n (foldS fb ft fd s)

recS :: ([a] -> b) -> (Int -> SliceExp a -> b -> b) -> (Int -> SliceExp a -> b -> b) -> SliceExp a -> b
recS fb ft fd (Base xs)  = fb xs
recS fb ft fd (Take n s) = ft n s (recS fb ft fd s)
recS fb ft fd (Drop n s) = fd n s (recS fb ft fd s)

materialize' :: SliceExp a -> [a]
materialize' = foldS id take drop

lenS' :: SliceExp a -> Int
lenS' = foldS length max (\n s -> min 0 (s - 1))

normalize' :: SliceExp a -> SliceExp a
normalize' = foldS Base normalizarTake normalizarDrop

-- foldS :: ([a] -> Int -> SliceExp a) -> 
--          (Int -> Int -> SliceExp a -> Int -> SliceExp a) -> 
--          (Int -> Int -> SliceExp a -> Int -> SliceExp a) -> 
--          SliceExp a -> Int -> SliceExp a

takeS' :: Int -> SliceExp a -> SliceExp a
takeS' = flip (foldS (\xs n -> Base (take n xs))
                    (\m s n -> Take m (s (min n m)))
                    (\m s n -> Drop m (s (n + m))))

-- demostracion lens . normalize = lens

por ppio de extensionalidad
para todo s. (lenS . normalize) s = lenS s ?
por def de (.)
para todo s. lenS (normalize s) = lenS s ?
sea slice de tipo SliceExp a, por ppio de induccion sobre la estructura de slice, es equivalente demostrar que:

Caso Base: slice = Base xs

lenS (normalize (Base xs)) = lenS (Base xs) ?

Caso Inductivo: slice = Take m s')

HI) lenS (normalize s') = lenS s' ?
TI) lenS (normalize (Take m s')) = lenS (Take m s') ?

Caso Inductivo 2: slice = Drop m s')

HI) lenS (normalize s') = lenS s' ?
TI) lenS (normalize (Drop m s')) = lenS (Drop m s') ?


Caso Base:

I)

lenS (normalize (Base xs))
     ---------------------
=                   def de normalize
lenS (Base xs)
--------------
=                   def de lenS
length xs

D)

lenS (Base xs)
--------------
=                   def de lenS
length xs

Caso Inductivo:

I)

lenS (normalize (Take m s'))
      ----------------------
=                   def de normalize
lenS (normalizarTake n (normalize s))
-------------------------------------
=                   def Lema lens

D)

lenS (Take m s')
----------------
=                   def de lenS
min n (lenS s)

























