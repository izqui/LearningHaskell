import Data.List

primes :: Integer -> [Integer]
primes 0 = []
primes n = map (\i-> (i*2)+1) ([1..n] \\ thelist n)

thelist :: Integer -> [Integer]
thelist n = map (\(i, j) -> i + j + 2*i*j) (prod [1..n] [1..n])

prod :: [a] -> [b] -> [(a, b)]
prod xs ys = [(x, y) | x <- xs, y <- ys]