-- https://sites.google.com/site/haskellufpel/

type Pessoa = (String, String, Int)
joao :: Pessoa
joao = ("joao da silva","222-2222", 17)

nome :: Pessoa -> String
nome (n,t,i) = n

telefone :: Pessoa -> String
telefone  (n,t,i) = t

idade :: Pessoa -> Int 
idade (n,t,i) = i

adcionaTupla :: (Int, Int) -> Int
adcionaTupla (a,b) = a+b

shift :: ((Int, Int), Int) -> (Int, (Int, Int))
shift ((a,b),c) = (a,(b,c))



-- EXERCICIO TABELA
vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 13
vendas 2 = 15
vendas 3 = 20
vendas _ = 0

vendasTotal :: Int -> Int
vendasTotal 0 = vendas 0
vendasTotal n = vendas n + vendasTotal (n-1)

semanaValor :: Int -> Int -> Int
semanaValor s n
	|vendas n == s = n
	|n>= 0 = semanaValor (s) (n-1)
	|otherwise = -1

cabeca :: String
cabeca = "semana\t       venda\n"


geraVendas :: Int -> String
geraVendas 0 = geraVenda 0
geraVendas n = geraVendas (n-1) ++ geraVenda n

geraVenda :: Int -> String
geraVenda n = "Semana " ++ show(n) ++ "\t" ++ show(vendas n) ++ "\n"

geraTotal :: Int -> String
geraTotal n = "Total\t\t" ++ show(vendasTotal n) ++ "\n"

tabela :: Int -> String
tabela n = cabeca ++ geraVendas n ++ geraTotal n 
