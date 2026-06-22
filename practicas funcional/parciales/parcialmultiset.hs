data MSExp a = EmptyMS | AddMS a (MSExp a) | RemoveMS a (MSExp a) 
                | UnionMS (MSExp a) (MSExp a) 
                | MapMS (a -> a) (MSExp a)

occursMSE :: Eq a => a -> MSExp a -> Int
occursMSE e mse = occursMESWith e id mse 

occursMESWith :: Eq a => a -> (a->a) -> MSExp a -> Int
occursMESWith e f EmptyMS          = 0
occursMESWith e f (AddMS x ms)     = unoSi (f x == e) + occursMESWith e f ms
occursMESWith e f (RemoveMS x ms)  = let n = occursMESWith e f ms
                                        in n - unoSi (n>0 && e == x)
occursMESWith e f (UnionMS ms ms') = occursMESWith e f ms + occursMESWith e f ms'
occursMESWith e f (MapMS f' ms)    = occursMESWith e (f . f') ms

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

filterMSE :: (a -> Bool) -> MSExp a -> MSExp a
filterMSE p EmptyMS         = EmptyMS
filterMSE p (AddMS x ms)    = if p x then AddMS x (filterMSE p ms) else filterMSE p ms 
filterMSE p (RemoveMS x ms) = if p x then RemoveMS x (filterMSE p ms) else filterMSE p ms
filterMSE p (UnionMS ms ms') = UnionMS (filterMSE p ms) (filterMSE p ms')
filterMSE p (MapMS f ms)    = MapMS f (filterMSE (p . f) ms)

isValidMSE :: Eq a => MSExp a -> Bool
isValidMSE EmptyMS          = True
isValidMSE (AddMS x ms)     = isValidMSE ms
isValidMSE (RemoveMS x ms)  = (occursMSE x ms) > 0 && isValidMSE ms     
isValidMSE (UnionMS ms ms') = isValidMSE ms && isValidMSE ms'
isValidMSE (MapMS f' ms)    = isValidMSE ms

evalMSE :: Eq a => MSExp a -> [a]
evalMSE EmptyMS          = []
evalMSE (AddMS x ms)     = x : evalMSE ms
evalMSE (RemoveMS x ms)  = eliminarE x (evalMSE ms)
evalMSE (UnionMS ms ms') = evalMSE ms ++ evalMSE ms'
evalMSE (MapMS f ms)    = map f (evalMSE ms)

eliminarE :: Eq a => a -> [a] -> [a]
eliminarE e []     = error "no se puede"
eliminarE e (x:xs) = if e == x then xs else x : eliminarE e xs

simpMSE :: Eq a => MSExp a -> MSExp a
simpMSE EmptyMS          = EmptyMS
simpMSE (AddMS x ms)     = AddMS x (simpMSE ms)
simpMSE (RemoveMS x ms)  = simpRemove x ms
simpMSE (UnionMS ms ms') = simpUnion ms ms'
simpMSE (MapMS f ms)     = simpMap f ms

simpRemove :: Eq a => a -> MSExp a -> MSExp a
simpRemove x (AddMS x' ms) = if x == x' then ms else RemoveMS x (AddMS x' ms)
simpRemove x ms            = RemoveMS x ms

simpUnion :: MSExp a -> MSExp a -> MSExp a
simpUnion EmptyMS e = e
simpUnion e EmptyMS = e
simpUnion ms ms'    = UnionMS ms ms'

simpMap :: (a -> a) -> MSExp a -> MSExp a
simpMap f EmptyMS = EmptyMS
simpMap f ms      = MapMS f ms

foldMSE :: b -> (a -> b -> b) -> (a -> b -> b) -> (b -> b -> b) -> ((a -> a) -> b -> b) -> MSExp a -> b
foldMSE fe fa fr fu fm EmptyMS          = fe 
foldMSE fe fa fr fu fm (AddMS x ms)     = fa x (foldMSE fe fa fr fu fm ms)
foldMSE fe fa fr fu fm (RemoveMS x ms)  = fr x (foldMSE fe fa fr fu fm ms)
foldMSE fe fa fr fu fm (UnionMS ms ms') = fu (foldMSE fe fa fr fu fm ms) (foldMSE fe fa fr fu fm ms')
foldMSE fe fa fr fu fm (MapMS f ms)     = fm f (foldMSE fe fa fr fu fm ms)

recMSE :: b -> (a -> MSExp a -> b -> b) -> (a -> MSExp a -> b -> b) -> (MSExp a -> MSExp a -> b -> b -> b) -> ((a -> a) -> MSExp a -> b -> b) -> MSExp a -> b
recMSE fe fa fr fu fm EmptyMS          = fe 
recMSE fe fa fr fu fm (AddMS x ms)     = fa x ms (recMSE fe fa fr fu fm ms)
recMSE fe fa fr fu fm (RemoveMS x ms)  = fr x ms (recMSE fe fa fr fu fm ms)
recMSE fe fa fr fu fm (UnionMS ms ms') = fu ms ms' (recMSE fe fa fr fu fm ms) (recMSE fe fa fr fu fm ms')
recMSE fe fa fr fu fm (MapMS f ms)     = fm f ms (recMSE fe fa fr fu fm ms)

occursMSE' :: Eq a => a -> MSExp a -> Int
occursMSE' e = occursMESWith' e id

occursMESWith' :: Eq a => a -> (a -> a) -> MSExp a -> Int
occursMESWith' e = flip (foldMSE (const 0)
                                 (\x n f  -> unoSi (f x == e) + n f)
                                 (\x n f  -> n f - unoSi (f x == e))
                                 (\n n' f -> n f + n' f)
                                 (\g n f  -> n (g . f)))

filterMSE' :: (a -> Bool) -> MSExp a -> MSExp a
filterMSE' p = foldMSE EmptyMS
                      (\x ms -> if p x then AddMS x ms else ms)
                      (\x ms -> if p x then RemoveMS x ms else ms)
                      (\ms ms' -> UnionMS ms ms')
                      (\g ms -> MapMS g ms)

isValidMSE' :: Eq a => MSExp a -> Bool
isValidMSE' = recMSE True
                    (\x ms b -> b)
                    (\x ms b -> (occursMSE x ms) > 0 && b)
                    (\ms ms' b b' -> b && b')
                    (\g ms b -> b)

-- evalMSE :: Eq a => MSExp a -> [a]
evalMSE' :: Eq a => MSExp a -> [a]
evalMSE' = foldMSE []
                  (\x xs -> x : xs)
                  (\x xs -> eliminarE x xs)
                  (\xs xs' -> xs ++ xs')
                  (\g xs -> map g xs)
