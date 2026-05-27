data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving(Show)

t = NodeT 1 
      (NodeT 2 
        (NodeT 4 EmptyT EmptyT) 
        (NodeT 5 EmptyT EmptyT)) 
      (NodeT 3 
        (NodeT 6 EmptyT EmptyT) 
        (NodeT 7 
          (NodeT 8 EmptyT EmptyT) 
          (NodeT 9 EmptyT EmptyT)))

losAntecesoresDe :: Ord a => a -> Tree a -> [a]
losAntecesoresDe e EmptyT          = []
losAntecesoresDe e (NodeT x t1 t2) = if e == x then []
                                                else if perteneceA e t1
                                                        then x : losAntecesoresDe e t1
                                                        else x : losAntecesoresDe e t2

perteneceA :: Eq a => a -> Tree a -> Bool
perteneceA e EmptyT          = False
perteneceA e (NodeT x t1 t2) = (e == x) || perteneceA e t1 || perteneceA e t2 

