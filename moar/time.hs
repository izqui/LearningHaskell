getTimeString :: [(String, Integer)] -> Integer -> String
getTimeString [] _ = "."
getTimeString ((s,i):xs) t
	| division > 0 = show division ++ s ++ (getTimeString xs $ mod t i)
	| otherwise = ""
	where division = div t i