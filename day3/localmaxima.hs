data List = N List Int List
		  | E

localmax :: [Integer] -> [Integer]
localmax (x:y:z:xs)
	| max y (max x z) == y = y:localmax xs
	| otherwise = localmax $ [y,z]++xs
localmax [_, _] = []
localmax _ = []