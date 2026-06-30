data Component = Cargo | Engine | Shield | Cannon deriving Show

data Spaceship = Module Component Spaceship Spaceship | Plug deriving Show

data Direction = Larboard | Starboard deriving Show

data Size = Small | Big | Torpedo deriving Show

type Hazard = (Direction, Int, Size) 

naveConMotores = Module Engine (Module Shield (Module Cargo Plug Plug) Plug) (Module Engine Plug Plug)

-- Ejercicio 1

-- a) que indica si la nave posee al menos un generador de campos de fuerza. 
shielded :: Spaceship -> Bool
shielded Plug            = False
shielded (Module c s s') = esShield c || shielded s || shielded s'

esShield :: Component -> Bool
esShield Shield = True
esShield _      = False

-- b) que indica si la nave posee al menos un canon.
armed :: Spaceship -> Bool
armed Plug            = False
armed (Module c s s') = esCannon c || armed s || armed s'

esCannon :: Component -> Bool
esCannon Cannon = True
esCannon _      = False

-- c) que retorna el poder de propulsion de una nave.
thrust :: Spaceship -> Int
thrust Plug                 = 0
thrust (Module c Plug Plug) = sumarSiEsMotor c
thrust (Module c s s')      = thrust s + thrust s'

sumarSiEsMotor :: Component -> Int
sumarSiEsMotor Engine = 1
sumarSiEsMotor _      = 0

-- d) que devuelve la nave resultante de desprender los modulos dependientes del modulo donde se recibe un impacto
-- (esta funcion asume que se produce el impacto).
wreck :: Hazard -> Spaceship -> Spaceship
wreck h s = let h' = dispararSi h (armed s)
            in if isSmallH h' && shielded s
                then s
                else wreck' h' s

dispararSi :: Hazard -> Bool -> Hazard
dispararSi (d,n, Big) True = (d,n, Small)
dispararSi h _             = h

isSmallH :: Hazard -> Bool
isSmallH (d,n, Small) = True
isSmallH h            = False

wreck' :: Hazard -> Spaceship -> Spaceship
wreck' _ Plug                     = Plug
wreck' (d, 1, s) (Module c s1 s2) = Module c Plug Plug
wreck' (d, n, s) (Module c s1 s2) = case d of
                                Larboard  -> Module c (wreck' (d, n-1, s) s1) s2
                                Starboard -> Module c s1 (wreck' (d, n-1, s) s2)

foldSS :: b -> (Component -> b -> b -> b) -> Spaceship -> b
foldSS fp fm Plug            = fp
foldSS fp fm (Module c s s') = fm c (foldSS fp fm s) (foldSS fp fm s')

recSS :: b -> (Component -> Spaceship -> Spaceship -> b -> b -> b) -> Spaceship -> b
recSS fp fm Plug            = fp
recSS fp fm (Module c s s') = fm c s s' (recSS fp fm s) (recSS fp fm s')

-- a
capacity :: Spaceship -> Int
capacity = foldSS 0 (\c n n' -> sumaSiEsEspacioCarga c + n + n')

sumaSiEsEspacioCarga :: Component -> Int
sumaSiEsEspacioCarga Cargo = 1
sumaSiEsEspacioCarga _     = 0

-- b) que dada una lista de naves retorna una de capacidad maxima.
largest :: [Spaceship] -> Spaceship
largest = recr (error "no hay nave para devolver")
                (\s sx r -> if null sx
                                then s
                                else largestBetween s r)

largestBetween :: Spaceship -> Spaceship -> Spaceship
largestBetween n n' = if capacity n > capacity n'
                            then n
                            else n'

recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr z f []     = z
recr z f (x:xs) = f x xs (recr z f xs)

-- laMasLarga :: [[a]] -> [a]
-- laMasLarga = recr (error "no hay lista para devolver") 
--                   (\xs xss r -> if null xs
--                                 then xs
--                                 else laMasLargaEntreDos xs r) 

-- laMasLargaEntreDos :: [a] -> [a] -> [a]
-- laMasLargaEntreDos xs ys = if length xs > length ys
--                             then xs
--                             else ys

-- c) que dada una nave retorna su alto y ancho (pensando el alto como la cantidad de componentes de la rama mas
-- larga y el ancho como la cantidad de componentes del nivel mas ancho).
dimensions :: Spaceship -> (Int,Int)
dimensions = appFork (heightSS, maximum . map length . sizeSS)

heightSS :: Spaceship -> Int
heightSS = foldSS 0 (\c s s' -> 1 + max s s')

sizeSS :: Spaceship -> [[Component]]
sizeSS = foldSS [] (\c s s' -> [c] : concatPerLevel s s')

appFork :: (a -> b, a -> c) -> a -> (b,c)
appFork (f,g) x = (f x, g x)

concatPerLevel :: [[a]] -> [[a]] -> [[a]]
concatPerLevel = foldr (\xs xss yss -> case yss of
                                        [] -> yss
                                        (ys: yss') -> (xs ++ ys) : xss yss) id

-- d)
manoeuvre :: Spaceship -> [Hazard] -> Spaceship
-- manoeuvre s = foldr (\h hs -> wreck' h s) s
manoeuvre = flip (foldr (\h hs s -> hs (wreck h s)) id)

test :: [Spaceship] -> [Hazard] -> [Spaceship]
-- test ss hs = filter (\nave -> thrust nave > 0) (map (\nave -> manoeuvre nave hs) ss)
-- test ss hs = filter (\nave -> thrust nave > 0) (map (flip manoeuvre hs) ss)
-- test ss hs = filter ((>0) . thrust) (map (flip manoeuvre hs) ss)
test = (filter ((>0) . thrust) .) . flip (map . flip manoeuvre)
-- test ss hs = filter ((>0) . thrust) (map (\nave -> manoeuvre nave hs) ss) 


components :: Spaceship -> [Component]
components Plug = []
components (Module c s1 s2) = components s1 ++ [c] ++ components s2

replace :: (Component -> Component) -> Spaceship -> Spaceship
replace f Plug = Plug
replace f (Module c s1 s2) = Module (f c) (replace f s1) (replace f s2)

para todo sp. componentes (replace f sp) = map f (componentes sp)
sea sp' un Spaceship, por ppio de induccion sobre la estructura de sp', es equivalente demostrar que:

Caso Base: sp' = Plug

componentes (replace f Plug) = map f (componentes Plug)

Caso Inductivo: sp' = (Module c s1 s2)

HI1) componentes (replace f s1) = map f (componentes s1) !
HI2) componentes (replace f s2) = map f (componentes s2) !
TI) componentes (replace f (Module c s1 s2)) = map f (componentes (Module c s1 s2))

Caso Base:

I)

componentes (replace f Plug) 
            ---------------
=                   def replace 
componentes Plug
----------------
=                   def de componentes
[]

D)

map f (componentes Plug)
      ------------------
=                   def de componentes
map f []
--------
=                   def de map
[]

Caso Inductivo:

I)

componentes (replace f (Module c s1 s2))
            ----------------------------
=                   def de replace
componentes (Module (f c) (replace f s1) (replace f s2))
--------------------------------------------------------
=                   def de componentes
componentes (replace f s1) ++ [f c] ++ componentes (replace f s2)
-------------------------              --------------------------
=                   HI1, HI2
map f (componentes s1) ++ [f c] ++ map f (componentes s2)

D)

map f (componentes (Module c s1 s2))
       ----------------------------
=                   def de componentes
map f (components s1 ++ [c] ++ components s2)
---------------------------------------------
=                   def map f (xs ++ ys) = map f xs ++ map f ys
map f (componentes s1) ++ [f c] ++ map f (componentes s2)
