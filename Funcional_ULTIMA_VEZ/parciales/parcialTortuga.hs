data Point = P Float Float

data Pen = NoColour | Colour Float Float Float

type Angle = Float

type Distance = Float

data Turtle = T Pen Angle Point

data TCommand = Go Distance
                | Turn Angle
                | GrabPen Pen
                | TCommand :#: TCommand -- :#: es un constructor INFIJO

recTC :: (Distance -> b) -> (Angle -> b) -> (Pen -> b) -> (TCommand -> TCommand -> b -> b -> b) -> TCommand -> b
recTC fgo ft fgrab fHash (Go d)      = fgo d
recTC fgo ft fgrab fHash (Turn a)    = ft a
recTC fgo ft fgrab fHash (GrabPen p) = fgrab p
recTC fgo ft fgrab fHash (t1 :#: t2) = fHash t1 t2 (recTC fgo ft fgrab fHash t1) (recTC fgo ft fgrab fHash t2)

foldTC :: (Distance -> b) -> (Angle -> b) -> (Pen -> b) -> (b -> b -> b) -> TCommand -> b
foldTC fgo ft fgrab fHash = recTC fgo ft fgrab (\ _ _ r1 r2 -> fHash r1 r2) 

-- foldTC fgo ft fgrab fHash (Go d)      = fgo d
-- foldTC fgo ft fgrab fHash (Turn a)    = ft a
-- foldTC fgo ft fgrab fHash (GrabPen p) = fgrab p
-- foldTC fgo ft fgrab fHash (t1 :#: t2) = fHash (foldTC fgo ft fgrab fHash t1) (foldTC fgo ft fgrab fHash t2)

cantSeq :: TCommand -> Int
cantSeq = foldTC (const 0) (const 0) (const 0) (\n1 n2 -> 1 + n1 + n2)

-- que dado un programa de gráficos de tortuga, describe la cantidad total de constructores (:#:) del mismo que están en algún argumento
-- izquierdo de un (:#:).
cantSeqsALaIzqDeSeq :: TCommand -> Int
cantSeqsALaIzqDeSeq = recTC (const 0) (const 0) (const 0) (\t1 t2 n1 n2 -> cantSeq t1 + n1 + n2 )

-- que dado un programa de gráficos de tortuga, describe uno equivalente que no tiene argumentos a la izquierda de un (:#:) que estén formados por (:#:).
assocDer :: TCommand -> TCommand
assocDer = foldTC Go Turn GrabPen (\t1 t2 -> assoc t1 t2)

assoc :: TCommand -> TCommand -> TCommand
assoc (Go d) t      = (Go d :#: t)
assoc (Turn a) t    = (Turn a :#: t)
assoc (GrabPen p) t = (GrabPen p :#: t)
assoc (t1 :#: t2) t = (assoc t (assoc t2 t))

-- expresada como recursión estructural implícita sobre TCommand. Esta función escala cada comando Go
-- de su argumento según un factor dado. Por ejemplo, scaleTC(Go 10 :#: Go 30) 2 = (Go 20 :#: Go 60).
scaleTC :: TCommand -> (Float -> TCommand) -- b :: (Float -> TCommand)
scaleTC = foldTC (\d float-> Go (float * d)) (\a float -> Turn a) (\p float -> GrabPen p) (\t1 t2 float -> t1 float :#: t2 float)

-- cantSeqs :: 
cantSeqs' (c1 :#: c2) = let (cs1, csis1) = cantSeqs' c1
                            (cs2, csis2) = cantSeqs' c2
                        in (1+cs1+cs2, cs1+csis1+csis2)
cantSeqs' c           = (0,0)

cantSeqs'' :: TCommand -> (Int,Int)
cantSeqs'' = foldTC (\d -> (0,0)) (\a -> (0,0)) (\p -> (0,0)) (\(n1,n1') (n2,n2') -> (1+n1+n2, n1+n1'+n2'))

