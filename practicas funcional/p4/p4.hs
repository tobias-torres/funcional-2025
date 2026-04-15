-- Ejercicio 1

suma x y = x + y 

-- a. udiv (x,y) = div x y -- Parcial: no se puede dividir por 0 
udivE (x,0) = error "No puedo dividir por 0" -- Parcial: no se puede dividir por 0 y no tiene otros casos para evaluar

udivE (x,y) = div x y -- Parcial: no se puede dividir por 0

udivH = uncurry div -- Parcial: no se puede dividir por 0

succ x = x + 1 -- Total: siempre devuelve un resultado para cualquier valor de x

succH = suma 1 -- Total: siempre devuelve un resultado para cualquier valor de x

porLaMitad = flip div 2 -- Total: devuelve un resultado porque siempre divide por 2

conDieresis 'u' = 'ü' 

conDieresisB 'u' = 'ü' 
-- conDieresisB c   = conDieresisB c 
-- i. conTildePM 'a' = 'á' 
-- conTildePM 'e' = 'é' 
-- conTildePM 'i' = 'í' 
-- conTildePM 'o' = 'ó' 
-- conTildePM 'u' = 'ú' 
-- j. conTildeE c = if esVocal c 
--                then conTildePM c 
--                else error "El valor recibido no es vocal" 
-- k. conTilde c = if esVocal c && esMinuscula c 
--               then conTildePM c 
--               else c 