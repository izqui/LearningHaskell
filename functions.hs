-- Testing
square x = x * x
doubleIfSmall x = if x < 100 then x*2 else x
wut x y = [1,x..y]
a x = 1

-- Double length function
ll :: [a] -> Integer
ll [] = 0
ll (x:xs) = 2 + ll xs

-- Crackle pod

cracklepodF :: Integer -> [Char]
cracklepodF 0 = show 0
cracklepodF n 
	| mod n 3 == 0 = "Crackle"
	| mod n 5 == 0 = "Pod"
	| otherwise = show n 

cracklepodL xs = [if mod x 15 == 0 then "Crackle Pod" else if mod x 3 == 0 then "Crackle" else if mod x 5 == 0 then "Pod" else show x | x <- xs]

-- Rigth triangles

triangles xs = [(a, b, c) | b <- xs, c <- xs, a <- xs, c<b, b<a, square a == square b + square c]

sumatorial :: Integer -> Integer
sumatorial 0 = 0
sumatorial 1 = 1
sumatorial n = n + sumatorial (n-1) 

funca :: Integer -> Integer -> Integer -> (Integer, Integer, Integer)
funca 0 0 x = (x, x, 0)
funca 0 x y = (x, 0, 0)
funca x y z = (0, 0, 0)