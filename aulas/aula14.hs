
somaTuplas :: [(Int,Int )] ->[Int]
somaTuplas l = [a+b | (a,b) <- l]

addOrdPairs :: [(Int,Int)]->[Int]
addOrdPairs l = [a+b | (a,b) <- l,a<b]

--LISTA 13

mapC ::(a->b)-> [a]-> [b]
mapC f l = [f x | x<-l]

filterC :: (a->Bool) ->[a]->[a]
filterC f l = [x | x<-l, f x]

removeEspacos :: String -> String
removeEspacos l = [x | x<-l, x/=' ']

sings:: [[a]] ->[a]
sings l = [x | x:xs<-l, length(x:xs) == 1]

matches :: Int -> [Int] -> [Int]
matches a l = [x | x<-l, x==a]

elemento :: Int -> [Int] -> Bool
elemento a l = length(matches(a)(l))/=0

divisores :: Int -> [Int]
divisores a = [x | x <-listaI(a)(a), mod a x ==0]

listaI :: Int ->Int -> [Int]
listaI 0 a = []
listaI x 0 = []
listaI 1 a = [1]
listaI  x a
	|mod x a == 0 = a : listaI (x) (a-1)
	|otherwise =  listaI (x)(a-1)

isPrime :: [Int]->[Int]
isPrime l = [x | x<- l,length(divisores(x))==2]

quickSortmenores :: Int->[Int] ->[Int]
menores a l = [x | x<- l, x<a]
 :: Ord Int => [Int]-> [Int]
quickSort (a:l) = quickSort (menores a l) ++ [a] ++ quickSort (maiores a l)

menores :: Int->[Int] ->[Int]
menores a l = [x | x<- l, x<a]

maiores :: Int->[Int] ->[Int]
maiores a l = [x | x<- l, x>a]
