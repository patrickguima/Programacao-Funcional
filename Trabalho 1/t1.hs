tamanhoLinha:: String -> Int
tamanhoLinha [] = 0
tamanhoLinha (x:xs)
	| x /= '\n' = 1 + tamanhoLinha xs
	|otherwise = tamanhoLinha xs

maiorLinha :: [Int]->Int
maiorLinha [] = 0
maiorLinha x = head(iSort3 x)

tamanhoLinhas :: [String] ->[Int]
tamanhoLinhas [] = []		
tamanhoLinhas (x:xs) = [tamanhoLinha x] ++ tamanhoLinhas xs

separaLinhas :: String -> [String]
separaLinhas [] = []
separaLinhas x
	|head(x) == '\n' = separaLinhas (tail 	x)
	|otherwise = [takeWhile ('\n'<) x] ++ separaLinhas (dropWhile ('\n'<) x)

iSort3 :: [Int] -> [Int]
iSort3 [] = []
iSort3 (x:xs) = ins3 x (iSort3 xs)

ins3 :: Int -> [Int] -> [Int]
ins3 x [] = [x]
ins3 a (x:xs) 
    |a >= x = (a:x:xs)
    |otherwise  = x: ins3 a (xs)

numeroPalavras :: String -> Int
numeroPalavras [] = 1
numeroPalavras (x:xs)
	|x == '\n' = 1
	|x == ' ' = 1 + numeroPalavras xs
	|otherwise = numeroPalavras xs

tamanhoPalavras :: String -> Int
tamanhoPalavras [] = 0
tamanhoPalavras (x:xs) = 1 + tamanhoPalavras xs 

divInt :: Int -> Int -> [Int]
divInt a 0 = [0]
divInt 1 1 = [1]
divInt a 1 = [a]
divInt a b = div a (b-1) : divInt (a - div a (b-1)) (b-1)

separaPalavras :: String -> [String]
separaPalavras [] = []
separaPalavras (x)
	|head(x) == ' ' = separaPalavras (tail 	x)
	|otherwise = [takeWhile (' '<) x] ++ separaPalavras (dropWhile (' '<) x)
 
insereEspacos :: [String]->[Int]->String
 --Lista de palavras -- tamanho de espaco a ser inserido
insereEspacos []  _  = []
insereEspacos (x:xs) (y:ys) = (x++aumentaString y) ++" "++ (insereEspacos xs ys)

aumentaString:: Int -> String
aumentaString 0 = []
aumentaString a = " " ++ aumentaString (a-1)

arrumaEspacos :: [String]-> Int -> String--Lista de frases --tamanho maior frase
arrumaEspacos [] n = []
arrumaEspacos (x:xs) n = insereEspacos (separaPalavras x) (divInt (n - (tamanhoLinha x)) (numeroPalavras x)) ++ "\n" ++ arrumaEspacos xs n

justifica :: String -> String
justifica [] = []
justifica x =arrumaEspacos (separaLinhas x) (maiorLinha (tamanhoLinhas (separaLinhas x)))

