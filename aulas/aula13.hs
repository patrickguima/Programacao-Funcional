--Bibliotecas importantes para o trabalho

-- System.console.ansi
--System,io

-------------------------------------------------------------------------	
	
data Lista a = Cons a (Lista a) | Vazio
	deriving(Eq,Show)



ex1 :: Lista Int
ex1 = Cons 1(Cons 2(Cons 3(Cons 4 Vazio)))

ex2 :: Lista Int
ex2 = Cons 5(Cons 6(Cons 7(Cons 9 Vazio)))

ex3 :: Lista (Lista Int)
ex3 = Cons (Cons 1( Cons 2 Vazio))(Cons (Cons 3(Cons 4 Vazio))(Cons Vazio Vazio))

tamanho :: Lista a -> Int
tamanho Vazio = 0
tamanho (Cons x xs) = 1 + tamanho xs


mapLista :: (a->b) -> Lista a -> Lista b
mapLista f Vazio = Vazio
mapLista f (Cons x xs) = Cons (f x) (mapLista f xs)

foldr1Lista :: (a->b->b) -> b -> Lista a -> b
foldr1Lista f y Vazio = y
foldr1Lista f y (Cons x xs) = (f x (foldr1Lista f y xs)
)
filterLista :: (a -> Bool) 	-> Lista a -> Lista a
filterLista f Vazio = Vazio
filterLista f (Cons x xs)
	|f x   = (Cons x (filterLista f xs))
	|otherwise  = filterLista f xs 

maisLista :: Lista a -> Lista a -> Lista a
maisLista Vazio Vazio = Vazio
maisLista Vazio (Cons x xs) = (Cons x xs)
maisLista (Cons x xs) Vazio = (Cons x xs)
maisLista (Cons x xs) (y)  = (Cons x(maisLista xs y)) 