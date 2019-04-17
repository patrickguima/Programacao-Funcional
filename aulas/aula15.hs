elemento n [] = False
elemento n (x:xs) = n == x || elemento n xs

data SocioClube = Socio Int String String

eqSocio :: SocioClube-> SocioClube ->Bool
eqSocio (Socio c1 _ _) (Socio c2 _ _) = c1 == c2

instance Eq SocioClube where
	(==) = eqSocio

showSocio :: SocioClube -> String
showSocio (Socio i s1 s2) = 
		"{"++ " codigo: " ++ show i ++
		", Nome: " ++  s1 ++ 
		", Fone: " ++  s2 ++ " }"

instance Show SocioClube where
	show = showSocio

data TUnica a = T1 a a

showTUnica :: Show a => TUnica a -> String
showTUnica (T1 a b) = "("++ show a ++"," ++ show b ++")"

instance Show a=> Show (TUnica a) where
	show = showTUnica 

data Tupla a b = T2 a b

showTupla :: (Show a, Show b) => Tupla a b -> String
showTupla (T2 a b) = "(" ++ show a ++ "," ++ show b ++ ")"

instance (Show a, Show b) => Show (Tupla a b) where
	show  = showTupla