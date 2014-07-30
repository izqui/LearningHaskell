import Data.List

--skips :: [a] -> [[a]]
--skips xs = doSkip 

doSkip :: Int -> [a] -> [a]
doSkip _ [] = []
doSkip i xs = head (drop i xs) : doSkip i (drop i xs) 