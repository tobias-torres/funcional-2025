twice :: (a->a) -> a -> a
twice f x = f (f x)

compose :: (b->c) -> (a->b) -> a -> c
compose f g x = f (g x)

compose12 :: (d->c) -> (a->b->d) -> a -> b -> c
compose12 f g x y = f (g x y)

-- compose12 :: (d1->c1) -> (a1-> b1-> d1) -> a1 -> b1 -> c1
-- twice     :: (a2 -> a2) -> a2 -> a2
-- -----------------------------------------------             d1 <- a2 -> a2,      c1 <- a2 -> a2
-- compose12 twice :: (a1 -> b1 -> a2 -> a2) -> a1 -> b1 -> a2 -> a2
-- compose         :: (b3 -> c3) -> (a3 -> b3) -> a3 -> c3
-- ----------------------------------------------------------------- a1 <- (b3 -> c3), b1 <- (a3 -> b3), a2 <- a3, a2 <- b3
-- compose12 twice compose :: (b3 -> a3) -> (a3 -> b3) -> a3 -> a3

funcion = \f g x -> f (g (f (g x)))

subst f g x = f x (g x)