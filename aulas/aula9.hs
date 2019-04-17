--tipos polimorfico / polimorfismo de tipos

tamanho :: [a] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

dobro n = n*2

mapa :: (a->b) ->[a]->[b]
mapa f [] = []
mapa f (x:xs) = f x : mapa f xs

-- LISTA 8

concatena :: [[a]] -> [a]
concatena [] = []
concatena (x:xs) =  x ++ concatena xs

inverte :: [a] -> [a]
inverte [] = []
inverte (x:xs) = inverte xs ++ [x]

ultimo :: [a] -> a
ultimo [] = error "deu ruim"
ultimo a = last a

inicio :: [a] -> [a]
inicio [] = []
inicio (x:xs) 
	|tamanho (x:xs) == 1 = []
	|otherwise  = x : inicio xs

take1 ::  Int -> [a] -> [a]
take1 n []  = []
take1 n (x:xs) 
	|n == 0 = []
	|otherwise = x : take1 (n-1) xs  

drop1 :: Int -> [a] -> [a]
drop1 n [] = []
drop1 n (x:xs)
	|n == 0 = (x:xs)
	|otherwise = drop1 (n-1) xs
