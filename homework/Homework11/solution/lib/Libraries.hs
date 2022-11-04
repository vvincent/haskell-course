module Libraries (isPrime) where

isPrime :: Int -> Bool
isPrime n = n `elem` take n primes
  where primes = filterPrime [2..]
        filterPrime [] = []
        filterPrime (p:xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]
