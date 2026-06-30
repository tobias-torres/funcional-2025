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
lenS (Take n s) = max n (lenS s)
lenS (Drop n s) = min 0 (lenS s - n)

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
takeS n (Drop m s) = Drop m (takeS (n+m) s) 
takeS n (Take m s) = Take m (takeS (min n m) s)

-- esquema primitivo y recursivo


-- demostracion lens . normalize = lens