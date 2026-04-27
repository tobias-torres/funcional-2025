-- Ejercicio 1)

-- -- a

-- Prop: doble = \x -> 2 * x ?
-- Dem: por ppio de ext, es equivalente demostrar que
--     ¿para todo x'. doble x' = (\x -> 2 * x) x' ?
-- sea n un numero cualquiera. Se vera que doble n = (\x -> 2 * x) n

-- Lado Izq:

-- doble n
-- -------
-- =               def de doble, x <- n
-- 2 * n


-- Lado Der:

-- (\x -> 2 * x) n
-- ---------------
-- =              def de beta reduccion, x <- n
-- 2 * n 


-- Prop: compose doble doble = cuadruple ?
-- Dem: por ppio de ext, es equivalente demostrar que
--     para todo x. compose doble doble x = cuadruple x ?
-- sea n un numero cualquiera. Se vera que compose doble doble n = cuadruple n

-- Lado Izq:

-- compose doble doble n
-- ---------------------
-- =                       def compose, f <- doble, g <- doble, x <- n
-- doble (doble n)
--        -------
-- =                       def doble, x <- n
-- doble (2 * n)
-- -------------
-- =                       def doble, x <- (2 * n)
-- 2 * (2 * n)
-- -----------
-- =                       aritm
-- 4 * n

-- Lado Der:

-- cuadruple n
-- -----------
-- =                       def cuadruple, n <- n
-- 4 * n

-- -- 2

-- Prop: para todo x. para todo y. x && y = not ((not x) || (not y)) ?

-- Por casos: 

-- x = True

-- para todo x. para todo y. True && y = not ((not True) || (not y)) ?
--                           ---------         
-- =                                                                   def de &&
-- para todo x. para todo y. True = not ((not True) || (not y)) ?
--                                         -------
-- =                                                                   def de not
-- para todo x. para todo y. True = not (False || (not y)) ?
--                                       ---------------
-- =                                                                   def de ||
-- para todo x. para todo y. True = not (not y)) ?
--                                  -----------
-- =                                                                   def de not
-- para todo x. para todo y. True = True ?



-- x = False

-- para todo x. para todo y. False && y = not ((not False) || (not y)) ?
--                           ----------      
-- =                                                                   def de &&
-- para todo x. para todo y. False = not ((not False) || (not y)) ?
--                                         ---------
-- =                                                                   def de not
-- para todo x. para todo y. False = not (True || (not y)) ?
--                                        ---------------
-- =                                                                   def ||
-- para todo x. para todo y. False = not True ?
--                                   --------
-- =                                                                   def de not
-- para todo x. para todo y. False = False ?
                                       
-- -- 3
-- -- a

-- Prop: curry suma' = suma?
-- Dem: por ppio de ext (dos veces), es equivalente demostrar que
--     para todo x. curry suma' x = suma x?
--     para todo x. para todo y. curry suma' x y = suma x y ?
-- sean n y m dos numeros, Se vera que curry suma' n m = suma n m

-- Lado Izq)

-- curry :: ((a,b) -> c) -> a -> b -> c 

-- curry suma' n m
-- ----------------
-- =                              def curry, f <- suma, x <- n, y <- m
-- suma' (n, m)
-- ------------
-- =                               def suma'
-- n + m

-- Lado Der)

-- suma n m
-- --------
-- =                               def suma
-- n + m

-- -- b

-- Prop: uncurry suma = suma'
-- Dem: por ppio de ext (dos veces), es equivalente demostrar que
--     para todo x. uncurry suma x = suma' x?
--     para todo x. para todo y. uncurry suma x y = suma' x y ?
-- sean n y m dos numeros, Se vera que uncurry suma n m = suma' n m

-- Lado Izq)

-- uncurry suma n m
-- ----------------
--                                 def de uncurry
-- suma (n, m)
-- -----------
-- =                               def de suma
-- n + m

-- Lado Der)

-- suma' n m
-- ---------
-- =                               def de suma'
-- n + m

-- -- 4

-- -- a

-- Prop: curry fst = const ?
-- Dem: por ppio de ext (dos veces), es equivalente demostrar que 
--     para todo x. curry fst x = const x ?
--     para todo x. para todo y. curry fst x y = const x y ?
-- sean n y m dos numeros, se vera que curry fst n m = const n m

-- Lado Izq)

-- curry fst n m
-- -------------
-- =                               def de curry
-- fst (n, m)
-- ----------
-- =                               def fst
-- n

-- Lado Der)

-- const n m
-- ---------
-- =                               def de const
-- n

-- -- b

-- Prop: uncurry (flip const) = snd ?
-- Dem: por ppio de ext, es equivalente demostrar que
--     para todo (x, y). uncurry (flip const) (x, y) = snd (x, y)?
-- sea (n, m) un par cualquiera, se vera que uncurry (flip const) (n, m) = snd (n, m)

-- Lado Izq:

-- uncurry (flip const) (x, y)
-- ---------------------------
-- =                          def de uncurry
-- (flip const) x y
-- ----------------
-- =                          def de flip
-- const y x
-- ---------
-- =                          def const
-- y

-- Lado Der:

-- snd (n, m)
-- ----------
-- =                          def de snd
-- m

-- -- 5
-- -- a

-- Dem: por ppio de ext tres veces, es equivalente demostrar que
--     para todo f. curry (uncurry f) = f ?
--     para todo f. para todo r. curry (uncurry f) r = f r ?
--     para todo f. para todo r. para todo s. curry (uncurry f) r s = f r s ?
-- sea g una funcion, (x, y) un par de elementos, se verá que curry (uncurry g) x y = g x y

-- Lado Izq)

-- curry (uncurry g) x y
-- ---------------------
-- =                           def de curry
-- (uncurry g) (x, y)
-- ------------------
-- =                           def de uncurry
-- g x y

-- Lado Der)

-- g x y

-- -- b

-- Dem: por ppio de ext tres veces, es equivalente demostrar que
--     para todo f'. uncurry (curry f') = f'?
--     para todo f'. para todo (r, s). uncurry (curry f') (r, s) = f' (r, s) ?
-- sea g una funcion, (x, y) un par de elementos, se verá que uncurry (curry g) (x, y) = g (x, y)

-- Lado Izq)

-- uncurry (curry g) (x, y)
-- ------------------------
-- =                           def de uncurry
-- (curry g) x y
-- -------------
-- =                           def de curry
-- g (x, y)

-- Lado Der)

-- g (x, y)

-- -- 6

-- Prop: appAssoc (uncurry (uncurry f)) = uncurry (compose uncurry f)?
-- Dem: por ppio de ext, es equivalente demostrar que
--     para todo (r,(s,t)). appAssoc (uncurry (uncurry f)) (r, (s,t)) = uncurry (compose uncurry f) (r, (s,t)) ?
-- sea (x, (y, z)) un par de elementos cualquiera, se verá que appAssoc (uncurry (uncurry f)) (x, (y,z)) = uncurry (compose uncurry f) (x, (y,z))

-- Lado Izq)

-- appAssoc (uncurry (uncurry f)) (x, (y,z))
-- -----------------------------------------
-- =                           def de appAssoc, f <- (uncurry (uncurry f)), p <- (x, (y, z))
-- (uncurry (uncurry f)) (assoc (x, (y, z)))
--                         -----------------
-- =                           def assoc, (x, (y, z)) <- (x, (y, z))
-- (uncurry (uncurry f)) ((x, y), z)
-- --------------------------------
-- =                           def uncurry, f <- (uncurry f), (x, y) <- ((x, y),z)
-- (uncurry f) (x, y) z
-- ------------------
-- =
-- f x y z

-- Lado Der)

-- uncurry (compose uncurry f) (x, (y,z))
-- --------------------------------------
-- =                           def uncurry, f <- (compose uncurry f), (x, y) <- (x, (y, z))
-- (compose uncurry f) x (y, z)
-- ---------------------
-- =                           def compose, f <- uncurry , g <- f, x <- x
-- uncurry (f x) (y, z)
-- --------------------
-- =                           def de uncurry, f <- f x, (x,y) <- (y, z)
-- f x y z

-- -- 7
-- -- a

-- succ, doble, const, id, subst, dup, etc

doble x = 2 * x

-- succ x = x + 1

apply f x = f x

subst f g x = f x (g x)

cuadruple = doble . doble

doble' = (*2) . id

twice f = f . f

many n f = f 