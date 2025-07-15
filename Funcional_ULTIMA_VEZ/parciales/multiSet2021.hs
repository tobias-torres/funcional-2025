data MSExp a = EmptyMS | AddMS a (MSExp a) | RemoveMS a (MSExp a) 
                | UnionMS (MSExp a) (MSExp a) 
                | MapMS (a -> a) (MSExp a)

occursMSE :: a -> MSExp a -> Int
occursMSE e ms = occursMSEWith e id ms

occursMESWith :: a -> (a->a) -> MSExp a -> Int
occursMESWith e f EmptyMS          = 0
occursMESWith e f (AddMS x ms)     = sumaSiEsIgual (e == f x) + occursMESWith e f ms
occursMESWith e f (RemoveMS x ms)  = let n in occursMESWith e f ms
                                        in n - sumaSiEsIgual (n>0 && e == x)
occursMESWith e f (UnionMS ms ms') = occursMESWith e f ms + occursMESWith e f ms'
occursMESWith e f (MapMS f' ms)    = occursMESWith e (f . f') ms

sumaSiEsIgual :: Bool -> Int
sumaSiEsIgual True  = 1
sumaSiEsIgual False = 0

filterMSE :: (a -> Bool) -> MSExp a -> MSExp a
filterMSE p EmptyMS         = EmptyMS
filterMSE p (AddMS x ms)    = if p x then AddMS x (filterMSE p ms) else filterMSE p ms 
filterMSE p (RemoveMS x ms) = if p x then RemoveMS x (filterMSE p ms) else filterMSE p ms
filterMSE p (MapMS f ms)    = MapMS f (filter (p . f) ms)

