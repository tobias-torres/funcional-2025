data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

arbolGrande :: Tree Int
arbolGrande = 
  NodeT 50
    (NodeT 25
      (NodeT 10
        (NodeT 5 EmptyT EmptyT)
        (NodeT 15 EmptyT EmptyT)
      )
      (NodeT 35
        (NodeT 30 EmptyT EmptyT)
        (NodeT 40 EmptyT EmptyT)
      )
    )
    (NodeT 75
      (NodeT 60
        (NodeT 55 EmptyT EmptyT)
        (NodeT 65 EmptyT EmptyT)
      )
      (NodeT 90
        (NodeT 85 EmptyT EmptyT)
        (NodeT 95 EmptyT EmptyT)
      )
    )


-- f EmptyT          = ...
-- f (NodeT x t1 t2) = ... f t1 ... f t2

heightT :: Tree a -> Int
heightT EmptyT          = 0
heightT (NodeT x t1 t2) = 1 + max (heightT t1) (heightT t2)

inOrder :: Tree a -> [a]
inOrder EmptyT          = []
inOrder (NodeT x t1 t2) = inOrder t1 ++ [x] ++ inOrder t2

mirrorT :: Tree a -> Tree a
mirrorT EmptyT          = EmptyT
mirrorT (NodeT x t1 t2) = NodeT x (mirrorT t2) (mirrorT t1)

levelN :: Int -> Tree a -> [a]
levelN n EmptyT          = []
levelN 0 (NodeT x t1 t2) = [x] 
levelN n (NodeT x t1 t2) =  (levelN (n-1) t1 ) ++ (levelN (n-1) t2)

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT x t1 t2) = if length (ramaMasLarga t1) > length (ramaMasLarga t2) then x : ramaMasLarga t1 else x : ramaMasLarga t2

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT          = []
todosLosCaminos (NodeT x EmptyT EmptyT) = [x]
todosLosCaminos (NodeT x t1 t2) = agregarRaiz x (todosLosCaminos t1 ++ todosLosCaminos t2)

agregarRaiz :: a -> [[a]] -> [[a]]
agregarRaiz e []       = []
agregarRaiz e (xs:xss) = (e : xs) : (agregarRaiz e xss)


arbolP :: Tree Int
arbolP = NodeT 10 (NodeT 102 (NodeT 1023 (NodeT 33 EmptyT EmptyT) EmptyT) EmptyT) EmptyT