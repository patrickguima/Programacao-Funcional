somaNaturais :: Int -> Int
somaNaturais 0 = 0
somaNaturais n = n + somaNaturais (n-1)


-- lista de exercicios 2
vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 0
vendas 2 = 25
vendas 3 = 20
vendas _ = 200

maximo :: Int -> Int -> Int
maximo a b
 	| a > b = a 
 	| otherwise = b

maiorVenda :: Int -> Int
maiorVenda 0 = vendas 0
maiorVenda n = maximo (vendas n) (maiorVenda (n-1))

maxVenda :: Int -> Int
maxVenda 0 = 0
maxVenda n 
	| maiorVenda n == vendas n = n
	| otherwise = maxVenda (n-1)

maxVenda2 :: Int -> Int -> Int
maxVenda2 m n 
	| maiorVenda n == vendas n = n
	| n < m = -1
	| otherwise = maxVenda2 (m)(n-1)

zeroVendas :: Int -> Int
zeroVendas n
 	| vendas n == 0 = n
 	| n >= 0  = zeroVendas (n-1)
 	| otherwise = -1

semanaValor :: Int -> Int -> Int
semanaValor s n
	|vendas n == s = n
	|n>= 0 = semanaValor (s) (n-1)
	|otherwise = -1

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)