tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (x:xs) = (tamanho xs) + 1  

--concatLista :: [[Int]] ->[Int]
--concatLista [[], []] = []
--concatLista [[a] [a]] = [a] : concatLista[[a][a]]

produtoLista :: [Int] -> Int
produtoLista [] = 1
produtoLista (x:xs) = (produtoLista xs) * x

dobraLista :: [Int] -> [Int]
dobraLista [] = []
dobraLista (x:xs) = (x * 2) : (dobraLista xs)

--------------------------------------
--ORDENAÇÃO INSERTION

iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins a (x:xs) 
    |a < x = (a:x:xs)
    |otherwise  = x: ins a (xs)


--ORDENAÇAO QUICK

qs :: [Int] -> [Int]
qs [] = []
qs (x:xs) = qs (menores x xs)
            ++ [x] ++
            qs (maiores x xs)

menores :: Int -> [Int] -> [Int]
menores a [] = []
menores a (x:xs)
    | x<= a = x: menores a (xs)
    |otherwise = menores a (xs)

maiores :: Int -> [Int] -> [Int]
maiores a [] = []
maiores a (x:xs)
	| x> a = x: maiores a (xs)
	|otherwise  = maiores a (xs)

--ORDENAÇÃO INSERTION SEM REPETIÇAO

iSort2 :: [Int] -> [Int]
iSort2 [] = []
iSort2 (x:xs) = ins2 x (iSort2 xs)

ins2 :: Int -> [Int] -> [Int]
ins2 x [] = [x]
ins2 a (x:xs) 
    |a < x = (a:x:xs)
    |a == x = x:xs
    |otherwise  = x: ins2 a (xs)


--ORDENAÇÃO INSERTION ORDEM DECRESCENTE

iSort3 :: [Int] -> [Int]
iSort3 [] = []
iSort3 (x:xs) = ins3 x (iSort3 xs)

ins3 :: Int -> [Int] -> [Int]
ins3 x [] = [x]
ins3 a (x:xs) 
    |a >= x = (a:x:xs)
    |otherwise  = x: ins3 a (xs)


-- RECEBE UMA LISTA E DEVOLVE O MAIOR E menores
minEmax :: [Int] -> (Int,Int)
minEmax [] = (0,0)
minEmax (x) = (menor (iSort x), maior (iSort3 x))


menor :: [Int] -> Int
menor [] = 0
menor (x:xs) = x

maior :: [Int] -> Int
maior [] = 0
maior (x:xs) = x


-- COM HEAD
minEmax2 :: [Int] -> (Int,Int)
minEmax2 [] = (0,0)
minEmax2  x  = (head(iSort x), head(reverse( iSort x)))


--LISTA 5

membro :: [Int] -> Int -> Bool
membro [] a = False
membro (x:xs) a
	|x == a = True
	|otherwise = membro (xs) a 

membroNum :: [Int] -> Int -> Int
membroNum [] a = 0
membroNum (x:xs) a 
	|a==x = 1 + membroNum (xs) a
	|otherwise = membroNum (xs) a


