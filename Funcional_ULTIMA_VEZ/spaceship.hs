data Component = Cargo | Engine | Shield | Cannon

data Spaceship = Module Component Spaceship Spaceship | Plug

data Direction = Larboard | Starboard

data Size = Small | Big | Torpedo

type Hazard = (Direction, Int, Size)

nave = Module Engine (Module Engine Plug Plug) Plug


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
thrust (Module c Plug Plug) = 1
thrust (Module c sp sp')    = thrust sp + thrust sp'

-- que devuelve la nave resultante de desprender los modulos dependientes del modulo donde se recibe un impacto
-- (esta funcion asume que se produce el impacto).
wreck :: Hazard -> Spaceship -> Spaceship
wreck 
wreck

wreck :: Hazard -> Spaceship -> Spaceship
wreck h s = let h' = shootHazardIf h (armed s)
             in if isSmallH h && shielded s then s else wreck' h' s

shootHazardIfArmed :: Hazard -> Bool -> Hazard
shootHazardIfArmed (d,n,Big) True = (d,n,Small)
shootHazardIfArmed h         _    = h

isSmallH (Small,_,_) = True
isSmallH _           = False

wreck' :: Hazard -> Spaceship -> Spaceship
wreck' _       Plug             = Plug
wreck' (d,1,s) _                = Plug
wreck' (d,n,s) (Module c s1 s2) = case d of
  Larboard  -> Module c (wreck' (d,n,s) s1) s2
  Starboard -> Module c s1 (wreck' (d,n,s) s2)