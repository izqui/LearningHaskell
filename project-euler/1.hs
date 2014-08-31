solve1 :: Integer -> [Integer] -> Integer
solve1 _ [] = 0
solve1 limit (x:xs) = (sum [0,x..limit]) + (solve1 limit xs) - (sum $ map (\a->sum [0,a..limit]) (multiprod x xs))

multiprod :: Integer -> [Integer] -> [Integer]
multiprod _ [] = []
multiprod a (x:xs) = a * x : multiprod a xs

result = solve1 999 [3,5]