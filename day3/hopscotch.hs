import Data.List

skips :: [a] -> [[a]]
skips xs = doSkip 1 [xs]

doSkip :: Int -> [[a]] -> [[a]]
doSkip i (x:xs)
	| i >= li = x:xs
	| otherwise = doSkip (i+1) (x:xs ++ [dropEach i x]) 
	where l = length x
	      li = l

dropEach :: Int -> [a] -> [a]
dropEach 0 x = x
dropEach _ [] = []
dropEach i xs = (take 1 (drop i xs)) ++ dropEach i (drop (i+1) xs)