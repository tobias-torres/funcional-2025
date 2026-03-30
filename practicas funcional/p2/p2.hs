first :: (a,b) -> a
first (x,y) = x 

apply :: (a -> b) -> a -> b
apply f = g 
  where g x = f x 

twice :: (a -> a) -> a -> a
twice f = g 
  where g x = f (f x) 

doble :: Int -> Int
doble x = 2 * x

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x) 

uflip :: ((b,a) -> c) -> (a, b) -> c
uflip f = g 
  where g p = f (swap p)

-- const :: a -> (b -> a)
-- const x = g
--     where g y = x

subst ::(a -> (b->c)) -> ((a ->b) -> (a -> c))
subst f = h
  where h g = k
          where k x = (f x)(g x)

suma :: Int -> Int -> Int
suma x y = x + y


funcionLoca = (\f -> f 2 + f 4)

yTambien :: Bool -> (Bool -> Bool)
yTambien b = if b then (\x -> x) else (\x -> False)

-- ejercicio 7

appFork :: (a -> b, a -> c) -> a -> (b, c)
appFork (f, g) = h 
  where h x = (f x, g x)

const :: a -> (b -> a)
const x = g  
  where g y = x 

appDup :: ((a,a) -> b ) -> (a -> b) 
appDup f = g 
  where g x = f (x, x) 
 
appPar :: (a -> b, c -> d) -> (a, c) -> (b, d)
appPar (f, g) = h 
  where h (x, y) = (f x, g y) 

appDist :: (a -> b) -> ((a, a) -> (b, b))
appDist f = g 
  where g (x, y) = (f x, f y) 

flip :: (a -> (b -> c)) -> (b -> (a -> c))
flip f = h 
  where h x = k 
          where k y = (f y) x 

subst :: (a -> (b -> c)) -> ((a -> b) -> (a -> c))
subst f = h 
  where h g = k 
          where k x = (f x) (g x)

-- 7.

-- a.
-- appFork (id,id)
-- \x -> (id x, id x)

-- b.
-- \f -> appDup (appDist f) 

-- \f -> (\x -> )


-- appDup id

-- \x -> id (x, x)

