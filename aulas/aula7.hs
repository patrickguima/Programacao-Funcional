somaPares :: [(Int,Int)] ->Int
somaPares [] = 0
somaPares (a:x) = c+d +somaPares x
	where
		c = fst a
		d = snd a
somaTripla :: [(Int,Int,Int)] -> Int
somaTripla [] = 0
somaTripla ((a,b,c):x) = a + b + c +somaTripla x

somaTupla :: [((Int,Int),(Int,Int))] ->Int
somaTupla [] = 0
somaTupla ((a,b):x) = somaPares [a] + somaPares [b] + somaTupla x

zipp :: [Int] ->[Int] -> [(Int,Int)]
zipp [] _ = []
zipp _ [] = []
zipp (a:x) (b:y) = [(a,b)] ++ zipp x y

zipp2 :: [Int] ->[Int]->[Int]-> [(Int,Int,Int)]
zipp2 [] _ _ = []
zipp2 _ [] _ = []
zipp2 _ _ [] = []
zipp2 (a:x) (b:y) (c:z) = [(a,b,c)] ++ zipp2 x y z

unZipp :: [(Int,Int)] -> ([Int],[Int])
unZipp [] = ([],[])
unZipp (x) = (unZippEsq x ,unZippDir x) 

unZippEsq :: [(Int,Int)] ->[Int]
unZippEsq [] = []
unZippEsq ((a,b):x) = [a] ++ unZippEsq x

unZippDir :: [(Int,Int)] ->[Int]
unZippDir [] = []
unZippDir ((a,b):x) = [b] ++ unZippEsq x
