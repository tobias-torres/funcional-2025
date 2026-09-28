data Point = P Float Float

data Pen = NoColour | Colour Float Float Float

type Angle = Float

type Distance = Float

data Turtle = T Pen Angle Point

data TCommand = Go Distance
                | Turn Angle
                | GrabPen Pen
                | TCommand :#: TCommand -- :#: es un constructor INFIJO

recT :: (Distance -> b) -> (Angle -> b) -> (Pen -> b) -> (TCommand -> TCommand -> b -> b -> b) -> TCommand -> b
recT fg ft fgr frec (Go d)      = fg d
recT fg ft fgr frec (Turn a)    = ft a
recT fg ft fgr frec (GrabPen p) = fgr p
recT fg ft fgr frec (t1 :#: t2) = frec t1 t2 (recT fg ft fgr frec t1) (recT fg ft fgr frec t2)

foldT :: (Distance -> b) -> (Angle -> b) -> (Pen -> b) -> (b -> b -> b) -> TCommand -> b
foldT fg ft fgr frec = 

recR :: b -> (a -> [a] -> b -> b) -> [a] -> b
recR z f []     = z
recR z f (x:xs) = f x xs (recR z f xs)

foldr' :: b -> (a -> b -> b ) -> [a] -> b
foldr' z f = recR z (\x _ r -> f x r)
