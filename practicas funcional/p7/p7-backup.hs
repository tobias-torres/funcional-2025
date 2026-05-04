type Nombre = String

data Planilla = Fin | Registro Nombre Planilla deriving (Show)

data Equipo = Becario Nombre | Investigador Nombre Equipo Equipo Equipo deriving (Show)

planilla = Registro "Mark" (Registro "Omar" (Registro "Tobias" Fin))
planillaB = Registro "Horace" (Registro "Mesut" Fin)

equipoDe7 :: Equipo
equipoDe7 = Investigador "Ana" 
                (Investigador "Beto" (Becario "Cami") (Becario "Dany") (Becario "Elio")) 
                (Becario "Fede") 
                (Becario "Gabi")


-- f Fin            = ...
-- f (Registro n p) = ... f p

-- f (Becario n)               = ...
-- f (Investigador n e1 e2 e3) = ... f e1 ... f e2 ... f e3

largoDePlanilla :: Planilla -> Int
largoDePlanilla Fin            = 0
largoDePlanilla (Registro n p) = 1 + largoDePlanilla p

-- b. esta, que toma un nombre y una planilla e indica si en la planilla dada está el nombre dado.
esta :: Nombre -> Planilla -> Bool
esta n Fin             = False
esta n (Registro n' p) = n == n' || esta n p

-- c. juntarPlanillas, que toma dos planillas y genera una única planilla con los registros de ambas planillas.
juntarPlanillas :: Planilla -> Planilla -> Planilla
juntarPlanillas Fin p             = p
juntarPlanillas (Registro n p) p2 = Registro n (juntarPlanillas p p2)

-- d. nivelesJerarquicos, que describe la cantidad de niveles jerárquicos de un equipo dado.
nivelesJerarquicos :: Equipo -> Int
nivelesJerarquicos (Becario n)               = 0
nivelesJerarquicos (Investigador n e1 e2 e3) = 1 + nivelesJerarquicos e1 + nivelesJerarquicos e2 + nivelesJerarquicos e3

-- e. cantidadDeIntegrantes, que describe la cantidad de integrantes de un equipo dado.
cantidadDeIntegrantes :: Equipo -> Int
cantidadDeIntegrantes (Becario n)               = 1
cantidadDeIntegrantes (Investigador n e1 e2 e3) = 1 + cantidadDeIntegrantes e1 + cantidadDeIntegrantes e2 + cantidadDeIntegrantes e3

-- f. planillaDeIntegrantes, que describe la planilla de integrantes de un equipo dado.
planillaDeIntegrantes :: Equipo -> Planilla
planillaDeIntegrantes (Becario n)               = Registro n Fin
planillaDeIntegrantes (Investigador n e1 e2 e3) = Registro n (juntarPlanillas (planillaDeIntegrantes e1) (juntarPlanillas (planillaDeIntegrantes e2) (planillaDeIntegrantes e3)))

