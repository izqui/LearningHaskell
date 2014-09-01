pairs :: [Char] -> [(Char, Maybe Char)]
pairs [] = []
pairs (x:[]) = [(x, Nothing)]
pairs (x:xs) = [(x, Just (last xs))] ++ (pairs $ init xs)

checkPairs :: [(Char, Maybe Char)] -> Bool
checkPairs [] = True
checkPairs ((_, Nothing):xs) = True && checkPairs xs
checkPairs ((a, Just b):xs) = a == b && checkPairs xs

isPalindrome :: Integer -> Bool
isPalindrome = checkPairs . pairs . show		  

biggestPalindrome :: Integer -> Integer -> Integer
biggestPalindrome from to = maximum $ filter isPalindrome $ (map (uncurry (*)) [(a,b) | a <- list, b <- list])
	where list = reverse [from..to]

result = biggestPalindrome 100 1000