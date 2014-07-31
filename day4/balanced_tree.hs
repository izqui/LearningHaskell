data Tree a = Leaf 
			| Node Integer (Tree a) a (Tree a) 
			deriving Show

foldTree :: [a] -> Tree a
foldTree = dofold 0

dofold :: Integer -> [a] -> Tree a
dofold _ [] = Leaf
dofold i (x:xs) =
	let half = div (length xs) 2 
	in Node i (dofold (i+1) (take half xs)) x (dofold (i+1) (drop half xs))
