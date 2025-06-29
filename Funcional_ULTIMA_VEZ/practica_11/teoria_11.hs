map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

succs = map' (\x -> x + 1)

lengthB :: [a] -> Int
lengthB []     = 0
lengthB (x:xs) = 1 + lengthB xs

sonCinco = foldr' (\n b -> n == 5 && b) True
sonCinco' = foldr' ((&&) . (==5)) True

cantTotal = foldr (\ys n -> length ys + n) 0
cantTotal' = foldr ((+) . length) 0

concatenar = foldr (\xs xss -> xs ++ xss ) []
concatenar' = foldr (++) []

sum' = foldr (+) 0
prod = foldr (*) 1
all = foldr (&&) True

mapC f = foldr (\x r -> f x : r) []
mapCBis f = foldr ((:) . f) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) = if p x then x : filter' p xs else filter' p xs

filterC p = foldr (\x r -> if p x then x : r else r ) []
-- filterCBis p = foldr ()

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z []     = z
foldr' f z (x:xs) = f x (foldr' f z xs)

takeF :: Int -> ([a] -> [a])
takeF n [] = []
takeF n (x:xs) = if n==0 then [] else x : takeF (n-1) xs

takeL :: Int -> [a] -> [a]
takeL = flip (foldr (\x h n -> if n==0 then [] else x : h (n-1)) (const []))

