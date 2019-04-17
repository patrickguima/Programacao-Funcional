-- LISTA EXTRA

--let l = ___?___
--  	in ______


retornaUltimo2 :: [a] -> a
retornaUltimo2 [] = error "erro"
retornaUltimo2 (x) = head(reverse(x))

retornaUltimo :: [Int] -> Int
retornaUltimo [] = 0
retornaUltimo (x) = head(reverse(x))

pegaPosicao :: Int -> [Int] -> Int
pegaPosicao a [] = 0
pegaPosicao 1 (x) = head(x)
pegaPosicao a (x:xs) = pegaPosicao (a-1) (xs) 

pega :: Int -> [Int] -> [Int]
pega _ [] = []
pega 1 x = [head x]
pega a (x:xs) = x : pega (a-1) xs

retira :: Int -> [Int] -> [Int]
retira _ [] = []
retira 1 (x:xs) = xs
retira a (x:xs) = retira (a-1) xs

mediaLista :: [Int] -> Int
mediaLista [] = 0
mediaLista (x:xs) =  let l = length xs
					in fromInteger((x + mediaLista xs) / l, )

