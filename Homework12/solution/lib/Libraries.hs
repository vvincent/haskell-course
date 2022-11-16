module Libraries (isPrime) where

primeNumbers :: Int -> [Int]
primeNumbers n = getPrimes [2] 3
  where getPrimes xs x
            | last xs > n = init xs
            | checkPrimality xs x = getPrimes (xs ++ [x]) (x + 1)
            | otherwise = getPrimes xs (x + 1)
        checkPrimality :: [Int] -> Int -> Bool
        checkPrimality xs x = length (filter (== 0) (map (rem x) xs)) == 0

isPrime :: Int -> Bool
isPrime n = n `elem` primeNumbers n
