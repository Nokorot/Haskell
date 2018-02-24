

isqrt :: Integer -> Integer
isqrt a = truncate ( sqrt (fromIntegral a)) 

{-

primes :: [Integer]
primes = 2 : (filter isPrime [3..])

lesPrimes :: Integer -> [Integer]
lesPrimes k = takeWhile (\p -> p < isqrt k) primes

isPrime :: Integer -> Bool
isPrime k = all (\a -> mod k a /= 0) (lesPrimes k)

-}

primes :: [Integer]
primes = 2 : ( filter (\k -> all (\a -> mod k a /= 0) (takeWhile (\p -> p < isqrt k) primes)  ) [3..] ) 
