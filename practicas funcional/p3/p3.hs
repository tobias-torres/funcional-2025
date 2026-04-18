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

-- -- id :: a -> a
-- -- id x = x

-- flip :: (a -> b -> c) -> b -> a -> c
-- flip f x y = f y x

-- uflip :: ((b, a) -> c) -> (a, b) -> c
-- uflip f p = f (swap p) 

-- const :: a -> b -> a
-- const x y = x 

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

-- 4.

-- (apply apply) apply :: (a -> b) -> a -> b

-- b. (twice doble) 2 :: Int

-- c. ((twice twice) twice) swap :: (a, a) -> (a, a)

-- d. ((flip twice) 1) doble :: Int

-- 5.

appDup = \f x -> f (x, x)
appDup'' f = \x -> f (x, x) 
appDup''' f x = f (x, x) 

appDup' = (\f -> (\x -> f (x, x)))

appFork = \(f, g) x -> (f x, g x) 

appFork' = (\(f, g) -> (\x -> (f x, g x))) 

appPar = \(f, g) (x, y) -> (f x, g y) 

appPar' = (\(f, g) -> (\(x, y) -> (f x, g y))) 

appDist = \f (x, y) -> (f x, f y) 

appDist' = (\f -> (\(x, y) -> (f x, f y))) 

subst = \f g x -> (f x) (g x)

subst' = (\f -> (\g -> (\x -> (f x) (g x))))

subst'' f = \g x -> f x (g x)
subst''' f g = \x -> f x (g x)
subst'''' f g x = f x (g x)

doble = \x -> 2 * x

-- 6.
-- a.
-- compose (fst snd) :: No tiene tipo
-- (compose fst) snd :: (x, (a, b)) -> a

-- compose :: (b' -> c) -> ((a' -> b') -> (a' -> c))
-- fst :: (a, b) -> a
-- --------------------------------------------- b' <-- (a, b), c <-- a
-- compose fst :: (a' -> (a, b)) -> (a' -> a)
-- snd :: (x, y) -> y
-- --------------------------------------------- a' <--(x, y); (a, b) <-- y
-- compose snd :: (x, (a, b)) -> a

-- b.
-- (uncurry curry snd) :: No tiene tipo

-- uncurry :: (a1 -> (b1 -> c1)) -> ((a1,b1) -> c1)
-- curry :: ((a2, b2) -> c2) -> (a2 -> (b2 -> c2)) 
-- --------------------------------------       a1 <-- (a2, b2) -> c2; b1 <-- (a2) ; c1 <-- (b2 -> c2)
-- uncurry curry       :: ((a2, b2) -> c2, a2) -> (b2 -> c2)
-- snd                 :: (a3, b3) -> b3
--------------------------------------------------------- 
-- (uncurry curry snd) :: No hay tipo

-- curry     :: ((a1, b1) -> c1) -> (a1 -> (b1 -> c1)) 
-- snd       :: (a2, b2) -> b2
-- -----------------------------------------------   a1 <-- a2, b1 <-- b2, c1 <-- b2
-- curry snd :: (a2 -> (b2 -> b2))

-- uncurry   :: (a1 -> (b1 -> c1)) -> ((a1,b1) -> c1)
-- curry snd :: (a2 -> (b2 -> b2))
-- --------------------------------------------------   a1 <-- a2, b1 <-- b2, c1 <-- b2
-- uncurry (curry snd) :: ((a2, b2) -> b2)

-- c.
            
-- (apply id) ((id apply) apply) :: (a -> b) -> (a -> b)

-- d.

-- compose (compose doble doble) :: (a1 -> Int) -> a1 -> Int

-- compose       :: (b1 -> c1) -> ((a1 -> b1) -> (a1 -> c1))
-- doble         :: Int -> Int
-- ---------------------------------------------------     b1 <-- Int, c1 <-- Int
-- compose doble :: (a1 -> Int) -> (a1 -> Int)
-- doble         :: Int -> Int
-- ---------------------------------------------------     a1 <-- Int
-- compose doble :: (Int -> Int)

-- compose                       :: (b1 -> c1) -> (a1 -> b1) -> a1 -> c1
-- compose doble doble           :: (Int -> Int)
-- --------------------------------------------------------------- 
-- compose (compose doble doble) :: (a1 -> Int) -> a1 -> Int

-- e.

-- (compose compose) doble doble :: 

-- compose         :: (b1 -> c1) -> ((a1 -> b1) -> (a1 -> c1))
-- compose         :: (b2 -> c2) -> ((a2 -> b2) -> (a2 -> c2))
-- ---------------------------------------------------         b1 <-- (b2 -> c2); c1 <-- (a2 -> b2) -> (a2 -> c2)
-- compose compose :: ((a1 -> (b2 -> c2)) -> (a1 -> (a2 -> b2) -> (a2 -> c2)))
-- doble           :: (Int -> Int)
-- --------------------------------------------------------------------------- a1 <-- Int, (b2 -> c2) <-- Int
-- (compose compose) doble :: Error de tipo

many :: Int -> (a -> a) -> a -> a 
many 0 f x = id x
many n f x = compose f (many (n-1) f) (id x)

many' :: Int -> (a -> a) -> a -> a 
many' 0 f = id
many' n f = compose f (many' (n-1) f)

manyR :: Int -> (a -> a) -> a -> a 
manyR 0 f = id
manyR n f = f . (manyR (n-1) f)

suma x y = x + y
-- 9.

-- a. cuadruple x = doble (doble x) 

cuadrupleI x = twice ((*) 2) x
cuadrupleII x = compose ((*) 2) ((*) 2) x
cuadrupleIII = twice ((*) 2)
cuadrupleIV = (doble . doble)
cuadrupleV x = flip twice x doble 


-- compose :: (b -> c) -> (a -> b) -> a -> c
-- compose f g x = f (g x)

-- flip :: (a -> b -> c) -> b -> a -> c
-- flip f x y = f y x

-- b. timesTwoPlusThree x = suma (doble x) 3 

timesTwoPlusThreeI x = flip suma 3 (doble x)
timesTwoPlusThreeII = ((suma 3) . doble) 
timesTwoPlusThreeIII = (subst (const (suma 3)) doble)



-- c. fourTimes f x = f (f (f (f x)))

fourTimesI f x = many 4 f x
fourTimesII f x = twice twice f x

