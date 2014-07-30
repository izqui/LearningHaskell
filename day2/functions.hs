factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1) 

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [x | x <- xs, f x] 

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ ac [] = ac
foldl' f ac (x:xs) = foldl' f (f ac x) xs

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
--map' f (x:xs) = f x : map' f xs
map' f xs = [b | a <- xs, let b = f a] 

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
	let smaller = quicksort (filter' (<=x) xs)
	    bigger = quicksort (filter' (>x) xs)
	in smaller ++ [x] ++ bigger
