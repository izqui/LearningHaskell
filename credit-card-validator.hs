toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x = map read [[x] | x <- show x] 

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev x = rev $ toDigits x

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:(y:(xs))) = [x, 2*y] ++ doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits x = sum (map sum (map toDigits x))

validate :: Integer -> Bool
validate x = ((sumDigits (doubleEveryOther (toDigitsRev x))) `mod` 10 == 0)

main = do putStrLn "Enter your credit card"
          x <- readLn
          if validate x
          	then putStrLn "Valid"
          else putStrLn "Invalid"
