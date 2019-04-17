data Arvore a = Folha a | No a (Arvore a) (Arvore a)
	deriving(Eq,Show)

arv1 :: Arvore Int
arv1 = No 3 (No 2 (Folha 4)(Folha 5)) (Folha 6)

arv2 :: Arvore Char
arv2 = No 'b' (Folha 'c') (No 'd' (Folha 'e') (No 'f' ( Folha 'g') (Folha 'h')))

somaArvore :: Arvore Int -> Int
somaArvore  (Folha x) = x
somaArvore (No x a1 a2) = x + (somaArvore a1) + (somaArvore a2)
	
multArvore :: Arvore Int -> Arvore Int
multArvore  (Folha x) = Folha (2*x)
multArvore (No x a1 a2) = (No (2*x) (multArvore a1) (multArvore a2))

contaArvore :: Arvore Int -> Int
contaArvore  (Folha x) = 1
contaArvore (No x a1 a2) = 1 + (contaArvore a1) + (contaArvore a2)

maiorArvore :: Arvore Int -> Int
maiorArvore (Folha x) = x
maiorArvore (No x a1 a2) = max (x)  (max (maiorArvore a1) (maiorArvore a2))

intNaArvore ::Arvore Int ->Int->Bool
intNaArvore (Folha x) a = a == x
intNaArvore (No x a1 a2) a
	|x == a =True
	|otherwise = (intNaArvore (a1) (a)) ||  (intNaArvore (a2)(a))

numVezes :: Arvore Int->Int-> Int
numVezes (Folha x) a
	| x==a = 1
	|otherwise =0
numVezes (No x a1 a2) a
	|x == a = 1 + (numVezes (a1) (a)) + (numVezes (a2)(a))
	|otherwise = (numVezes (a1) (a)) + (numVezes (a2)(a))


refleteArvore ::Arvore a -> Arvore a
refleteArvore (Folha x) = Folha x
refleteArvore (No x a1 a2) = (No x) (refleteArvore a2) (refleteArvore a1)

alturaArvore :: Arvore a -> Int
alturaArvore (Folha x) = 0
alturaArvore (No x a1 a2) = 1 + max (alturaArvore a1) (alturaArvore a2)

arvToList :: Arvore Int -> [Int]
arvToList (Folha x) = [x]
arvToList (No x a1 a2) = [x] ++ (arvToList a1) ++ (arvToList a2)	
