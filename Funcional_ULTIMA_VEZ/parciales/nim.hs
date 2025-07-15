data Nim = Empty -- juego vacío
        | Heap Int Nim deriving (Show)

type Move = (Int,Int) -- jugada: de la fila i remover k fichas

data GameTree = Nil -- árbol vacío
                | Node (Move, GameTree) -- (jugada, hijos - jugadas del contrincante)
                GameTree -- hermanos (otras jugadas propias)

-- 1

foldNim :: b -> (Int -> b -> b) -> Nim -> b
foldNim fe fh Empty        = fe
foldNim fe fh (Heap n nim) = fh n (foldNim fe fh nim)

recNim :: b -> (Int -> Nim -> b -> b) -> Nim -> b
recNim fe fh Empty        = fe
recNim fe fh (Heap n nim) = fh n nim (recNim fe fh nim)

foldGame :: b -> ((Move, b) -> b -> b) -> GameTree -> b
foldGame fnil fnode Nil              = fnil
foldGame fnil fnode (Node (jugada, gt) gt') = fnode (jugada, foldGame fnil fnode gt) (foldGame fnil fnode gt')

recGame :: b -> ((Move, b) -> GameTree -> b -> b) -> GameTree -> b
recGame fnil fnode Nil              = fnil
recGame fnil fnode (Node (jugada, gt) gt') = fnode (jugada, recGame fnil fnode gt) gt (recGame fnil fnode gt)

-- 2.a

heaps :: Nim -> Int
heaps = foldNim 0 (\p n -> 1 + n)

chips :: Nim -> Int
chips = foldNim 0 (\p n -> p + n)

maxHeap :: Nim -> Int
maxHeap = foldNim 0 max

-- alongside' :: Nim -> Nim -> Nim
-- alongside' Empty nim = nim
-- alongside' nim Empty = nim
-- alongside' (Heap n nim) nim' = Heap n (alongside' nim nim') 

alongside :: Nim -> (Nim -> Nim)
alongside nim nim' = foldNim nim' (\f n -> Heap f n) nim

nim = Heap 1 (Heap 3 (Heap 4 Empty))
nim2 = Heap 2 (Heap 6 Empty)

gameHeight :: GameTree -> Int
gameHeight = foldGame 0 (\(move, n) n' -> max (n+1) n')

branches :: GameTree -> [[Move]]
branches = foldGame [[]] (\(move, hs) rs -> map (move :) (hs ++ rs))

-- 3

turn :: Nim -> Move -> Maybe Nim
turn Empty (fila, fichas)        = Nothing
turn (Heap n nim) (fila, fichas) = if fila == 0 
                                    then restarFichas fichas nim
                                    else Nothing

restarFichas :: Int -> Nim -> Maybe Nim
restarFichas n Empty = Nothing
restarFichas n (Heap m nim) = if (n - m > 0) then Just (Heap (n - m) nim) else Nothing 

moves :: Nim -> [Move]
moves Empty        = []
moves (Heap n nim) = jugadasPosibles n (heaps nim) ++ moves nim

jugadasPosibles :: Int -> Int -> [Move]
jugadasPosibles 0 pila = []
jugadasPosibles n pila = (pila, n) : jugadasPosibles (n-1) pila

