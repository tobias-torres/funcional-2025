data Component = Cargo | Engine | Shield | Cannon deriving (Show)

data Spaceship = Module Component Spaceship Spaceship | Plug deriving (Show)

data Direction = Larboard | Starboard

data Size = Small | Big | Torpedo

type Hazard = (Direction, Int, Size)

nave = Module Engine (Module Shield Plug Plug) (Module Engine Plug Plug)

nave2 = Module Cargo (Module Engine Plug Plug) Plug
nave3 = Module Cargo (Module Cargo Plug Plug) (Module Cargo Plug Plug)

naves = [nave, nave2,nave3]

naveLarga = Module Engine
              (Module Shield
                (Module Cargo
                  (Module Cannon Plug Plug)
                  Plug)
                (Module Engine Plug Plug))
              (Module Cannon Plug Plug)

-- que indica si la nave posee al menos un generador de campos de fuerza
shielded :: Spaceship -> Bool
shielded Plug              = False
shielded (Module c sp sp') = esCampoDeFuerza c || shielded sp || shielded sp'

esCampoDeFuerza :: Component -> Bool
esCampoDeFuerza Shield = True
esCampoDeFuerza _      = False

-- que indica si la nave posee al menos un cannon.
armed :: Spaceship -> Bool
armed Plug              = False
armed (Module c sp sp') = esCannon c || armed sp || armed sp'

esCannon Cannon = True
esCannon _      = False

-- que retorna el poder de propulsion de una nave.
thrust :: Spaceship -> Int
thrust Plug                 = 0
thrust (Module c Plug Plug) = sumarSiesMotor c
thrust (Module c sp sp')    = thrust sp + thrust sp'

sumarSiesMotor :: Component -> Int
sumarSiesMotor Engine = 1
sumarSiesMotor _      = 0

-- que devuelve la nave resultante de desprender los modulos dependientes del modulo donde se recibe un impacto
-- esta funcion asume que se produce el impacto.
wreck :: Hazard -> Spaceship -> Spaceship
wreck h s = checkAttack (nerfHazard h (armed s)) s

checkAttack :: Hazard -> Spaceship -> Spaceship
checkAttack h s = if isSmallHazard h && shielded s
                      then s
                      else massiveAttack h s

massiveAttack :: Hazard -> Spaceship -> Spaceship
massiveAttack _ Plug = Plug
massiveAttack (d, 1, s) _ = Plug
massiveAttack (d, n, s) (Module c s1 s2) = case d of 
                                            Larboard -> Module c (massiveAttack (d, n-1, s) s1) s2
                                            Starboard -> Module c s1 (massiveAttack (d, n-1, s) s2)

nerfHazard :: Hazard -> Bool -> Hazard
nerfHazard (d, n, Big) True = (d, n, Small)
nerfHazard h _              = h

isSmallHazard :: Hazard -> Bool 
isSmallHazard (d, n, Small) = True
isSmallHazard _             = False

-- ## Ejercicio 2 ##

foldSS :: b -> (Component -> b -> b -> b) -> Spaceship -> b
foldSS fp fm Plug            = fp
foldSS fp fm (Module c s s') = fm c (foldSS fp fm s) (foldSS fp fm s')

-- ## Ejercicio 3 ##

capacity :: Spaceship -> Int
capacity = foldSS 0 (\c n n' -> esCarga c + n + n')

esCarga :: Component -> Int
esCarga Cargo = 1
esCarga _     = 0

largest :: [Spaceship] -> Spaceship
largest = foldr maximumCapacity Plug

maximumCapacity :: Spaceship -> Spaceship -> Spaceship
maximumCapacity sp sp' = if capacity sp > capacity sp'
                            then sp
                            else sp'

-- que dada una nave retorna su alto y ancho (pensando el alto como la cantidad de componentes
-- de la rama mas larga y el ancho como la cantidad de componentes del nivel mas ancho).
dimensions :: Spaceship -> (Int,Int)
dimensions = appFork (heightSS, maximum . map length . componentsPerLevel)

heightSS :: Spaceship -> Int
heightSS = foldSS 0 (\c s1 s2 -> 1 + max s1 s2)

appFork :: (a -> b, a -> c) -> a -> (b, c)
appFork (f, g) x = (f x, g x)

componentsPerLevel :: Spaceship -> [[Component]]
componentsPerLevel = foldSS [] (\c s1 s2 -> [c] : concatPerLevel s1 s2)

concatPerLevel :: [[a]] -> ([[a]] -> [[a]])
concatPerLevel = recr (\yss -> yss) (\xs xss xss' yss -> case yss of
                                        [] -> xss
                                        (ys:yss') -> (xs ++ ys) : xss' yss') 


recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr z f []     = z
recr z f (x:xs) = f x xs (recr z f xs)

-- que simula el resultado de maniobrar una nave a traves de una serie de peligros. Si se encuentra un objeto
-- pequeno y la nave esta escudada no se produce impacto. Si el objeto es grande y la nave esta armada entonces
-- se transforma en un objeto pequeno. Si es un torpedo no se puede evitar el impacto
manoeuvre :: Spaceship -> ([Hazard] -> Spaceship)
manoeuvre = flip (foldr (\h hs s -> hs (wreck h s)) id)

-- test :: [Spaceship] -> [Hazard] -> [Spaceship]
-- test 


Demuestre que:

para todo sp:Spaceship . componentes (replace f sp) = map f (componentes sp)
sea s, de tipo Spaceship. Por ppio de induccion sobre la estructura de s, es equivalente demostrar que:

Caso Base: s = Plug

componentes (replace f Plug) = map f (componentes Plug)?

Caso Inductivo: s = Module c s1 s2)

HI1) componentes (replace f s1) = map f (componentes s1)!
HI2) componentes (replace f s2) = map f (componentes s2)!
TI) componentes (replace f (Module c s1 s2)) = map f (componentes (Module c s1 s2)) ?

Caso Base

LI)

componentes (replace f Plug)
            ----------------
=                           def replace
componentes Plug    
----------------
=                           def componentes
[]

LD)

map f (componentes Plug)
       -----------------
=                           def componentes
map f []
--------
=                           def map
[]


Caso inductivo:

LI)

componentes (replace f (Module c s1 s2)) 
            ---------------------------
=                           def replace
componentes (Module (f c) (replace f s1)(replace f s2))
-------------------------------------------------------
=                           def componentes
componentes (replace f s1) ++ [f c] ++ componentes (replace f s2)
--------------------------               --------------------------
=                           HI1, HI2
map f (componentes s1) ++ [f c] ++ map f (componentes s2)
-----------------------------------------------------------
=                           lema map-distributivo
map f (componentes s1 ++ [c] ++ (componentes s2))

LD)

map f (componentes (Module c s1 s2))
      -----------------------------
=                           def componentes
map f (componentes s1 ++ ([c] ++ componentes s2))
-----------------------------------------------
=                           lema map-distributivo
map f (componentes s1 ) ++ map f ([c] ++ componentes s2)
                            ----------------------------
=                           lema map-distributivo
map f (componentes s1) ++ map f [c] ++ map f (componentes s2)


LEMA: map-distributivo

para todo xs. para todo ys. map f xs ++ map f ys = map f (xs ++ ys) ?
sean zs e ws dos listas finitas. Por ppio de induccion sobre la estructura de zs, es equivalente demostrar que:

Caso Base: zs = []

map f [] ++ map f ws = map f ([] ++ ws)?

Caso Inductivo: zs = (z:zs')

HI) map f zs' ++ map f ws = map f (zs' ++ ws) !
TI) map f (z:zs') ++ map f ys = map f ((z:zs') ++ ys) ?

CASO BASE:

LI)

map f [] ++ map f ws
--------
=                   def ++
[] ++ map f ws
--------------
=                   def ++
map f ws

LD)

map f ([] ++ ws)
        ---------
=                   def ++
map f ws


Caso Inductivo:

LI)

map f (z:zs') ++ map f ys
-------------
=                  def map
f z : map f zs' ++ map f ys

LD)

map f ((z:zs') ++ ys)
        ------------
=                  def ++
map f (z : (zs' ++ ys))
-----------------------
=                  def map
f z : (map f (zs' ++ ys))
       -----------------
=                  HI
f z : map f zs' ++ map f ys


componentes :: Spaceship -> [Component]
componentes Plug = []
componentes (Module c s1 s2) = componentes s1 ++ [c] ++ componentes s2

replace :: (Component -> Component) -> Spaceship -> Spaceship
replace f Plug = Plug
replace f (Module c s1 s2) = Module (f c) (replace f s1) (replace f s2)

map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : (map' f xs)

-- (++) :: [a] -> [a] -> [a]
-- (++) []     ys = ys
-- (++) xs     [] = xs
-- (++) (x:xs) ys = x : (xs (++) ys)