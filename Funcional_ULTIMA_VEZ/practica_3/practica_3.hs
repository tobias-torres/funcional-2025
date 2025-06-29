-- PRACTICA 3

-- 1

curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (x, y) = f x y

-- 2 & 3

swap (x, y) = (y, x)

apply :: (a -> b) -> a -> b
apply f x = f x 

twice :: (a -> a) -> a -> a
twice f x = f (f x) 

id' :: a -> a
id' x = x 

flip' :: (b -> a -> c) -> a -> b -> c
flip' f x y = f y x 

uflip :: ((b,a) -> c) -> (a,b) -> c 
uflip f p = f (swap p) 

const' :: a -> b -> a
const' x y = x 

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x) 

-- 4

doble x = 2 * x

-- :: (a -> b) -> a -> b
ej1 = (apply apply) apply 

-- :: Int
-- twice doble 2 

-- swap :: (a, b) -> (b, a)
-- twice twice twice swap :: (a, a) -> (a, a)

-- ()
-- flip twice 1 doble :: Int

-- 5 Reescribir las siguientes definiciones utilizando sólo lambdas (sin ​where ni let)

appDup = \f x -> f (x, x) 

appFork = \(f, g) x -> (f x, g x)

appPar = \(f, g) x y -> (f x, g y) 

appDist = \f x y -> (f x, f y) 

subst = \f g x -> (f x) (g x)

-- 6 

-- compose (fst snd) NO tiene tipo, porque no podes usar como argumento de fst una funcion
-- compose fst snd , no encontre

-- uncurry (curry snd) ::  (a, b) -> b, acomode los parentesis del (curry snd)

-- (apply id) ((id apply) apply) :: (a -> b) -> a -> b

-- compose (compose doble doble) :: (a -> Int) -> a -> Int

-- (compose compose) doble doble No tiene tipo

-- 7

many :: Int -> (a -> a) -> a -> a 
many 0 f x = x 
many n f x = f (many (n-1) f x)

-- compose :: (b -> c) -> (a -> b) -> a -> c
-- compose f g x = f (g x) 

many' :: Int -> (a -> a) -> a -> a
many' 0 f = id
many' n f = compose f (many (n-1) f)

many'' :: Int -> (a -> a) -> a -> a
many'' 0 f = id
many'' n f = f . (many (n-1) f)

-- 8

-- a. (Int -> Int) -> (Int -> Int)
-- Castellano: una funcion que toma una funcion de int->int y devuelve otra funcion de int-> int

-- (Int -> Int) -> Int -> Int
-- Frances: una funcion que toma una funcion de int-> int y otra otro int, para devolverte un int

-- b. (a -> (b -> c)) -> (a -> b) -> c 
-- Castellano: una funcion que toma un elemento a y devuelve una funcion, que despues toma otra funcion de a->b y me devuelve un c

-- (a -> b -> c) -> (a -> b) -> c 
-- Frances: es una funcion que toma dos funciones, una de a->b->c y otra de a->b , me devuelve un c

-- c. (a -> b, c -> d) -> ((a, c) -> (b, d)) 
-- Castellano: una funcion que toma un par de funciones y me devuelve una funcion que toma una tupla y devuelve una tupla transformada.

-- (a -> b, c -> d) -> (a, c) -> (b, d) 
-- Frances: es una funcion que toma una tupla de funciones, otra tupla y me devuelve una tupla transformada.

-- d. ((a, a) -> b) -> (a -> b) 
-- Castellano:una funcion que toma una tupla y me devuelve un tipo b, donde despues devuelve una funcion que toma un a y devuelve un b

-- ((a, a) -> b) -> a -> b 
-- Frances: una funcion que toma una funcion de una tupla en b y un parametro a, me devuelve un b

-- e. (a -> (b -> c)) -> (b -> (a -> c)) 
-- Castellano: es una funcion que toma un a y me devuelve una funcion de a en b, que a su vez toma una funcion que toma un b y me devuelve una funcion de a en c.

-- (a -> b -> c) -> b -> a -> c 
-- Frances: una funcion que toma un a y un b, para devolverme un c, despues toma un b y un a y me devuelve un c

-- f. (a -> b) -> ((a, a) -> (b, b)) 
-- Castellano: una funcion que toma un a y me devuelve un b, que a su vez, toma una funcion que toma una tupla de a 

-- (a -> b) -> (a, a) -> (b, b)
-- Frances: es una funcion que toma una funcion de a en b, una tupla de a y me devuelve una tupla de b

-- g. (a -> b, a -> c) -> (a -> (b, c)) 
-- Castellano: una funcion que toma una tupla de funciones donde una transforma un a en b y la otra un a en c, me devuelve una funcion
-- que toma un a y me devuelve una tupla de (b,c)

-- (a -> b, a -> c) -> a -> (b, c)
-- Frances: una funcion que toma una tupla de funciones y un a, para devolverme una tupla de (b,c)

-- h. (a -> (b -> c)) -> ((a -> b) -> (a -> c)) 
-- Castellano: una funcion que toma un a y me devuelve una funcion que transforma un b en c, me devuelve una funcion que transforma un a en b, en un a en c

-- (a -> b -> c) -> (a -> b) -> a -> c
-- Frances: una funcion que toma 2 parametros un a y un b, para devolverme un c, despues toma una funcion de a en b y por ultimo toma un a, donde todo esto me devuelve un c

-- i. a -> (b -> a) 
-- Castellano: es una funcion donde toma un a y me devuelve una funcion que transforma un b en un a

-- a -> b -> a 
-- Frances:  una funcion que toma 2 parametros un a y un b y me devuelve un a

-- 9

suma x y = x + y

succ x = x + 1

-- a. cuadruple x = doble (doble x) 

versionUno = compose doble doble 1

versionDos = compose (twice doble) id 1

versionTres = flip' const' 3 (twice doble) 1

versionCuatro = many'' 2 doble 2

-- b. timesTwoPlusThree x = suma (doble x) 3 

versionUno' = flip' suma 3 (doble 2)

versionDos' = compose (suma 3) doble 2

-- c. fourTimes f x = f (f (f (f x)))

versionUno'' = twice twice doble 2

versionDos'' = many'' 4 doble 2

versionTres'' = compose twice twice doble 2