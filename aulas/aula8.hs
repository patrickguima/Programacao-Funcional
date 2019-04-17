-- LISTA 7

times2 n = 2*n
times3 n = 3*n

mapInt :: (Int -> Int) ->[Int] -> [Int]
mapInt f [] = []
mapInt f (a:x)  = f a : mapInt f x

dobra l = map times2 l
triplo l = map times3 l

vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 13
vendas 2 = 15
vendas 3 = 20
vendas _ = 0

total :: (Int -> Int) -> Int -> Int
total f 0 = f 0
total f a = f a + total f (a-1)

soma ::  Int -> Int -> Int
soma x y = x+y

foldInt :: (Int->Int -> Int) -> [Int] -> Int
foldInt f [a] = a
foldInt f (a:xs) = f a (foldInt f xs)

filterString :: (Char -> Bool) -> [Char] -> [Char]
filterString f [] = []
filterString f (a:xs)
	|f a = [a] ++ filterString f xs 
	|otherwise  = filterString f xs

naoEspaco :: Char -> Bool
naoEspaco x = x/= ' '


somaQuad ::  [Int] -> Int
somaQuad [] = 0
somaQuad a =foldInt soma (mapInt times2 a)

testaVal :: (f)->[Int] -> Bool
testaVal f [] = False
testaVal f a = testa (f a)

testa :: [Int] -> Bool
testa [] = True
testa (a:xs)
	|a < 0 = False
	|otherwise  = testa xs 