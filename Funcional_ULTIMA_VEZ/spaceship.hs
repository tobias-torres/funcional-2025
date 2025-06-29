data Component = Cargo | Engine | Shield | Cannon deriving (Show)

data Spaceship = Module Component Spaceship Spaceship | Plug deriving (Show)

data Direction = Larboard | Starboard

data Size = Small | Big | Torpedo

type Hazard = (Direction, Int, Size)

nave = Module Engine (Module Shield Plug Plug) (Module Engine Plug Plug)

nave2 = Module Cargo (Module Engine Plug Plug) Plug

nave3 = Module Cargo (Module Cargo Plug Plug) (Module Cargo Plug Plug)

naveLarga = Module Engine (Module Shield (Module Cargo (Module Cannon Plug Plug) Plug) (Module Engine Plug Plug))
                          (Module Cannon Plug Plug)

naves = [nave, nave2,nave3]



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
-- wreck :: Hazard -> Spaceship -> Spaceship
-- wreck h spaceShip = wreck' h spaceShip

-- wreck' :: Hazard -> Spaceship -> Spaceship
-- wreck' 

-- ## Ejercicio 2 ##

foldSS :: b -> (Component -> b -> b -> b) -> Spaceship -> b
foldSS fp fm Plug            = fp
foldSS fp fm (Module c s s') = fm c (foldSS fp fm s) (foldSS fp fm s')

recSS :: b -> (Component -> Spaceship -> Spaceship -> b -> b -> b) -> Spaceship -> b
recSS fp fm Plug            = fp
recSS fp fm (Module c s s') = fm c s s' (recSS fp fm s) (recSS fp fm s')

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
-- dimensions :: Spaceship -> (Int,Int)
-- dimensions = appFork (heightSS, )

heightSS :: Spaceship -> Int
heightSS = foldSS 0 (\c s1 s2 -> 1 + max s1 s2)

componentPerLevel :: Spaceship -> [[Component]]
componentPerLevel = 

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT x t1 t2) = [x] : concatPerLevel (listPerLevel t1) (listPerLevel t2)

concatPerLevel :: [[a]] -> [[a]] -> [[a]]
concatPerLevel [] yss            = yss
concatPerLevel xss []            = xss
concatPerLevel (xs:xss) (ys:yss) = (xs ++ ys) : concatPerLevel xss yss

appFork (f, g) x = (f x, g x)