-- Importante aca!

                        -- f         g
expresion = (==0) . ((`mod` 2) . (*2))

-- ((==0) . (mod 2) . (*2)) 3
-- (==0) (((mod 2) . (*2)) 3)
-- (==0) ( (mod 2) ((*2) 3) )
-- (==0) ((mod 2) 6)
-- (==0) (6 mod 2)
-- (==0) 0
-- True


-- (==) :: Eq a => a -> a -> Bool
-- mod :: Int -> Int -> Int
-- (*) :: Int -> Int -> Int


-- infixl Operan a izquierda y infixr operan a derecha
-- Primero vienen las funciones prefijas (como even, map, etc.) y luego los chirimbolos:
-- infixr 9  .
-- infixl 9  !!
-- infixr 8  ^, ^^, **
-- infixl 7  *, /, `quot`, `rem`, `div`, `mod`, :, %
-- infixl 6  +, -
-- infixr 5  :
-- infixr 5  ++
-- infix  4  ==, /=, <, <=, >=, >, `elem`, `notElem`
-- infixr 3  &&
-- infixr 2  ||
-- infixr 0  $

swap (a, b) = (b, a)

uflip :: ((b, a) -> c) -> (a, b) -> c
uflip f p = f (swap p) 

-- a.
-- uflip . uncurry

-- (.)       :: (b -> c) -> ((a -> b) -> (a -> c))
-- uflip     :: ((b1,a1) -> c1) -> ((a1, b1) -> c1)
------------------------------------------------ b <-- (b1, a1) -> c1, c <-- (a1, b1) -> c1
-- (.) uflip         :: (a -> ((b1, a1) -> c1)) -> (a -> (a1, b1) -> c1)
-- uncurry           :: (a2 -> (b2 -> c2)) -> ((a2, b2) -> c2)
------------------------------------------------------------- a <-- (a2 -> (b2 -> c2)), (b1, a1) -> c1 <-- ((a2, b2) -> c2)
-- (.) uflip uncurry ::  (a2 -> b2 -> c2) -> (b2, a2) -> c2


-- -- b.
-- flip . uncurry
-- (.)      :: (b1 -> c1) -> ((a1 -> b1) -> (a1 -> c1))
-- flip     :: (a2 -> (b2 -> c2)) -> (b2 -> (a2 -> c2))
-- ----------------------------------------------------- b1 <-- (a2 -> (b2 -> c2)), c1 <-- (b2 -> (a2 -> c2))
-- (.) flip :: (a1 -> (a2 -> (b2 -> c2))) -> (a1 -> (b2 -> (a2 -> c2)))
-- uncurry  :: (a3 -> (b3 -> c3)) -> ((a3, b3) -> c3)
-- ----------------------------------------------------- a1 <-- (a3 -> (b3 -> c3)), a2 <-- (a3, b3),  b2 -> c2 <-- c3
-- (.) flip uncurry :: (a3 -> b3 -> (b2 -> c2)) -> b2 -> (a3, b3) -> c2

-- -- c.
-- uflip . (uncurry . (flip . uncurry))

-- (.)     :: (b1 -> c1) -> ((a1 -> b1) -> (a1 -> c1))
-- uncurry :: (a2 -> (b2 -> c2)) -> ((a2, b2) -> c2)
-- ----------------------------------------------------- b1 = (a2 -> (b2 -> c2)) , c1 = ((a2, b2) -> c2)
-- (.) uncurry      :: (a1 -> (a2 -> (b2 -> c2))) -> (a1 -> ((a2, b2) -> c2))
-- (.) flip uncurry :: (a3 -> b3 -> (b2 -> c2)) -> (b2 -> ((a3, b3) -> c2))
-- ----------------------------------------------------- a1 = (a3 -> b3 -> (b2 -> c2)), 
--                                                    -- a2 = b2
--                                                    -- b2 = ((a3, b3)
                                                      -- c2 = c2)
(.) uncurry (.) flip uncurry :: (a3 -> (b3 -> (b2 -> c2))) -> ((b2, (a3, b3)) -> c2) 
(.) uflip                    :: (a -> ((b1, a1) -> c1)) -> (a -> ((a1, b1) -> c1))
----------------------------------------------------------------------------------
                                                                            -- a3 = (a -> ((b1, a1) -> c1))
                                                                            -- b3 = a
                                                                            -- b2 = (a1, b1)
                                                                            -- c2 = c1
resultado                    :: (a1, b1), ((a -> ((b1, a1) -> c1), a) ) -> c1

-- -- d.
-- uncurry . flip

-- -- e.
-- uncurry :: a2 -> (b2 -> c2)) -> ((a2, b2) -> c2)