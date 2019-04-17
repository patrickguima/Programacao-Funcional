idade :: Int
idade = 17

maiorIdade :: Bool
maiorIdade = (idade>=18)  -- () opcional

quadrado :: Int->Int
quadrado x = x * x

mini :: Int -> Int -> Int
mini a b
	| a <=b 	= a -- / chama-se GUARDA
	| otherwise = b

mi :: Int -> Bool --  maior idade
mi i = i>=18

igual :: Int -> Int -> Bool
igual x y = 	x == y



-- LISTA 1 

igualQuatro :: Int -> Int -> Int ->Int -> Bool   
igualQuatro x y w z  = (x==y) && (y==z) && (z==w)

quantosIguais :: Int -> Int -> Int ->Int
quantosIguais a b c
	|(a==b) && (b==c) = 3
	|(a==b) && (b/=c) = 2
	|(a/=b) && (b==c) = 2
	|otherwise = 0

todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes a b c = (a/=b) && (b/=c) && (a/=c)

todosIguais :: Int -> Int -> Int -> Bool
todosIguais x y z = (x==y) && (y==z) && (x==z)

quantosSaoigual :: Int -> Int -> Int ->Int 
quantosSaoigual a b c 
	|todosDiferentes a b c = 0
	|todosIguais  a b c = 3
	|otherwise = 2

elevadoQuatro :: Int -> Int
elevadoQuatro x = x*x*x*x

vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 13
vendas 2 = 15
vendas 3 = 20
vendas _ = 0

vendasTotal :: Int -> Int
vendasTotal 0 = vendas 0
vendasTotal n = vendas n  + vendasTotal (n-1)