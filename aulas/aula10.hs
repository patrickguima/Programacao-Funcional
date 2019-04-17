--funçoes de alta ordem
-- map,filter, foldr    -- funçoes contidas no haskell

map1 :: (a->b) -> [a] -> [b]
map1 f [] = []
map1 f (x:xs) = f x: map1 f xs

filter1:: (a->Bool) ->[a]->[a]
filter1 f [] = []
filter1 f (x:xs)
	|f x = x:filter1 f xs
	|otherwise  = filter1 f xs

maior :: Int -> Bool
maior a
	| a < 5  = True
	|otherwise = False

foldr2 :: (a->b->b) ->b ->[a]->b
foldr2 f e [] = e
foldr2 f e (x:xs) = f x (foldr2 f e xs)

--LISTA 9
--ex1

concat1 ::[[Int]] -> [Int]
concat1 x = foldr (++) [] x  

con :: [Int] -> [Int] -> [Int]
con a b = a ++b
--ex2
and1 :: [Bool]->Bool
and1 x = foldr (&&) True x

--ex3
pares :: [Int] ->[Int]
pares x = filter (par) x

par:: Int->Bool
par a = mod a 2== 0 

--ex4

invert :: [Int] -> Int
invert x = foldr (+) 0 x  