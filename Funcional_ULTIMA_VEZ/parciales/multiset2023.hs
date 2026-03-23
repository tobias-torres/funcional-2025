data N = Z | S N

data MSExp a = EmptyMS 
                | Entry N a
                | AddAll N (MSExp a) 
                | Union (MSExp a) (MSExp a) 

evalMSE :: Eq a => MSExp a -> a -> N
evalMSE EmptyMS x          = Z
evalMSE (Entry n x') x     = if x' == x then n else Z 
evalMSE (AddAll n mse) x   = addN n (evalMSE mse x) 
evalMSE (Union mse mse') x = addN (evalMSE mse x) (evalMSE mse' x)

updateOccursMES :: (N -> N) -> MSExp a -> MSExp a
updateOccursMES f EmptyMS          = EmptyMS
updateOccursMES f (Entry n x)      = Entry (f n) x
updateOccursMES f (Union mse mse') = Union (updateOccursMES f mse) (updateOccursMES f mse')

normMSE :: MSExp a -> MSExp a
normMSE EmptyMS          = EmptyMS
normMSE (Entry n x)      = (Entry n x)
normMSE (AddAll n mse)   = updateOccursMES (addN n) (normMSE mse)
normMSE (Union mse mse') = normalizar mse mse'

normalizar :: MSExp a -> MSExp a -> MSExp a
normalizar EmptyMS m = m
normalizar n EmptyMS = n
normalizar n m       = Union n m

cantFallas :: MSExp a -> Int
cantFallas EmptyMS          = 0
cantFallas (Entry n x)      = 0
cantFallas (AddAll n mse)   = 1 + cantFallas mse
cantFallas (Union mse mse') = unoSiEmpty mse + unoSiEmpty mse' + cantFallas mse + cantFallas mse'

unoSiEmpty :: MSExp a -> Int
unoSiEmpty EmptyMS = 1
unoSiEmpty _       = 0       

addN :: N -> N -> N
addN Z m     = m
addN (S n) m = S (addN n m)

-- 3

foldMS :: b -> (N -> a -> b) -> (N -> b -> b) -> (b -> b -> b) -> MSExp a -> b
foldMS femp fent fadd funi EmptyMS          = femp
foldMS femp fent fadd funi (Entry n x)      = fent n x
foldMS femp fent fadd funi (AddAll n mse)   = fadd n (foldMS femp fent fadd funi mse)
foldMS femp fent fadd funi (Union mse mse') = funi (foldMS femp fent fadd funi mse) (foldMS femp fent fadd funi mse')

recMS :: b -> (N -> a -> b) -> (N -> MSExp a -> b -> b) -> (MSExp a -> MSExp a -> b -> b -> b) -> MSExp a -> b
recMS femp fent fadd funi EmptyMS          = femp
recMS femp fent fadd funi (Entry n x)      = fent n x
recMS femp fent fadd funi (AddAll n mse)   = fadd n mse (recMS femp fent fadd funi mse)
recMS femp fent fadd funi (Union mse mse') = funi mse mse' (recMS femp fent fadd funi mse) (recMS femp fent fadd funi mse')

evalMSE' :: Eq a => MSExp a -> a -> N -- b :: (a -> N)
evalMSE' = foldMS (const Z) (\n x x' -> if x' == x then n else Z) (\n mse x' -> addN n (mse x') ) (\h h' x' -> addN (h x') (h' x'))

updateOccursMES' :: (N -> N) -> MSExp a -> MSExp a
updateOccursMES' = flip (foldMS (const EmptyMS) (\n x f -> Entry (f n) x) (\_ _ -> error "no se puede") (\mse mse' f -> Union (mse f) (mse' f)))

normMSE' :: MSExp a -> MSExp a
normMSE' = foldMS EmptyMS (\n x -> Entry n x) (\n mse -> updateOccursMES (addN n) mse) (\mse mse' -> normalizar mse mse')

cantFallas' :: MSExp a -> Int
cantFallas' = recMS 0 (\n x -> 0) (\n mse r -> 1 + r) (\mse mse' r1 r2 -> unoSiEmpty mse + unoSiEmpty mse' + r1 + r2)

-- data N = Z | S N

-- data MSExp a = EmptyMS 
--                 | Entry N a
--                 | AddAll N (MSExp a) 
--                 | Union (MSExp a) (MSExp a) 