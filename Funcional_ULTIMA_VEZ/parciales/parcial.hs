data Color = Rojo | Verde

data Dir = IZQ | DER


data Potenciador = B | Izq | Der | IzqDer

data Ficha = F Jugador Color Int -- El Int son los puntos de la ficha

data Jugada = J [Dir] Ficha

data ModRama = NoOp | PonerY (Color, Int) (Dir, ModRama) -- Color y puntaje de la celda a crear | Direccion a la que seguir modificando el Treeble

data Treeble = H (Maybe Ficha) | N Color Potenciador (Maybe Ficha) Treeble Treeble

data Jugador = JRojo | JVerde
-- No recuerdo como era Jugador, lo unico importante es que:
-- Un jugador tenia un color
-- Un jugador es el mismo que otro si tiene el mismo color, ya que el juego es un 1vs1

---------------------------------------------------------------
    -- Una jugada es valida si:
    --   Es hoja y esta libre
    --   Es hoja, y si la ficha a jugar tiene estrictamente mas puntos que la presente
    --   Es nodo, el nodo es del mismo color que la ficha y no hay ficha
    --   Es nodo, el nodo es del mismo color que la ficha y la ficha jugada es tiene mas puntos que la ficha presente
jugadaValida :: Jugada -> Treeble -> Bool
jugadaValida j (H mbf)           = case mbf of 
                                    Nothing -> True
                                    Just f  -> fichasDeJugada j > puntosDeFicha f
jugadaValida j (N c p mbf t1 t2) = igualColor (colorFicha (jugadaFicha j)) c && case mbf of 
                                                                        Nothing -> True
                                                                        Just f  -> (fichasDeJugada j) > puntosDeFicha f

jugadaFicha :: Jugada -> Ficha
jugadaFicha (J xs f) = f

colorFicha :: Ficha -> Color
colorFicha (F j c n) = c

fichasDeJugada :: Jugada -> Int
fichasDeJugada (J xs f) = puntosDeFicha f

puntosDeFicha :: Ficha -> Int
puntosDeFicha (F j c n) = n

igualColor :: Color -> Color -> Bool
igualColor Rojo Rojo   = True
igualColor Verde Verde = True
igualColor _ _         = False

--     retorna el puntaje de un jugador, siendo este calculado como la suma de puntos del jugador en TODO el arbol siguiendo las siguientes reglas:
--       Si la ficha esta en una hoja y es del jugador, suma los puntos de la ficha
--       Si la ficha esta en un nodo y es del jugador, suma el puntaje de la ficha multiplicado por:
--          El puntaje del subarbol izq o derecho si el potenciador es Izq o Der respectivamente
--          La suma de los puntajes de ambos subarboles si el potenciador es IzqDer
--          Ningun extra si potenciador es B
-- puntaje :: Jugador -> Treeble -> Int
-- puntaje


-- armarT :: [ModRama] -> Treeble
-- Construye un Treeble en base a la lista de ModRama dado

-- extenderT :: Treeble -> [Dir] -> [ModRama] -> Treeble
-- Recorre el treeble segun las direcciones hasta que no haya mas.
-- Luego, extiende el Treeble segun la lista de ModRama recibida

---------------------------------------------------------------
-- para todo j, para todo t. puntaje j t >= 0
---------------------------------------------------------------
-- definir fold y recr sobre las estructuras correspondientes
---------------------------------------------------------------