data Point = P Float Float

data Pen = NoColour | Colour Float Float Float

type Angle = Float

type Distance = Float

data Turtle = T Pen Angle Point

data TCommand = Go Distance
                | Turn Angle
                | GrabPen Pen
                | TCommand :#: TCommand -- :#: es un constructor INFIJO

recTC :: Float -> 
recTC fgo ft fgrab fHash (Go d)      = fgo d
recTC fgo ft fgrab fHash (Turn a)    = ft a
recTC fgo ft fgrab fHash (GrabPen p) = fgrab p
recTC fgo ft fgrab fHash (t1 :#: t2) = fHash (recTC fgo ft fgrab fHash t1) (recTC fgo ft fgrab fHash t2)