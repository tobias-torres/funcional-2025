-- 1

curry :: ((a,b) -> c) -> a -> b -> c 
curry f x y = f (x,y)
-- curryRico = \f x y -> f (x, y)


uncurry :: (a -> b -> c) -> (a,b) -> c 
uncurry f (x, y) = f x y
-- uncurryRico = \f (x, y) -> f x y

-- 2. y 3.

swap (a, b) = (b, a)

apply :: (a -> b) -> a -> b
apply f x = f x 
  
twice :: (a -> a) -> a -> a
twice f x = f (f x) 

id :: a -> a
id x = x

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

uflip :: ((b, a) -> c) -> (a, b) -> c
uflip f p = f (swap p) 

const :: a -> b -> a
const x y = x 

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)