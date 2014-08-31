fib :: [Integer]
fib = fibb 0 1

fibb :: Integer -> Integer -> [Integer]
fibb a b = next:fibb b next
		where next = a+b

solve2 :: Integer -> [Integer] -> Integer
solve2 limit = sum . filter even . takeWhile (<=limit)

result = solve2 4000000 fib