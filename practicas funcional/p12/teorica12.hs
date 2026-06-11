data GTree a = GNode a [GTree a]

sumGT :: GTree Int -> Int
sumGT (GNode x ts) = x + sum (map sumGT ts)

sumGTPO :: GTree Int -> Int
sumGTPO (GNode x ts) = x + aRaTyS ts

aRaTyS :: [GTree Int] -> Int -- Aplicar recursión a todos y sumar
aRaTyS [] = 0
aRaTyS (t:ts) = sumGTPO t + aRaTyS ts

