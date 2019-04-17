inverte :: [Int]->[Int]
inverte x = foldr  (cola) [] x

cola :: Int ->[Int]-> [Int]
cola x y  = y ++[x]
