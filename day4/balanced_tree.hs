data Tree a = Leaf 
			| Node Integer (Tree a) a (Tree a) 
			deriving Show

data Child = LeftNode | RightNode | RandomNode deriving Show

foldTree :: [a] -> Tree a
foldTree = dofold 0

dofold :: Integer -> [a] -> Tree a
dofold _ [] = Leaf
dofold i (x:xs) =
	let half = div (length xs) 2 
	in Node i (dofold (i+1) (take half xs)) x (dofold (i+1) (drop half xs))

checkFold :: Tree a -> Bool
checkFold x = abs ((lastTree LeftNode x) - (lastTree RightNode x)) <= 1

lastTree :: Child -> Tree a -> Integer 
lastTree _ Leaf = 0
lastTree RightNode (Node _ _ _ n) = 1 + lastTree RightNode n
lastTree LeftNode (Node _ n _ _) = 1 + lastTree LeftNode n