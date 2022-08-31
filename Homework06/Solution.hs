

-- Question 1
-- Write a function that takes in an integer n and returns a list of the first n
-- numbers of the fibonacci list (https://en.wikipedia.org/wiki/Fibonacci_number). 

fib :: Int -> [Int]
fib n = addElement [1,1]
  where addElement xs = 
          if length xs == n then xs 
          else addElement (xs ++ [last xs + last (init xs)])

-- Question 2
-- Write a function that takes in an integer n, calculates the factorial n! and 
-- returns a string in the form of 1*2* ... *n = n! where n! is the actual result. 

factorial :: Int -> String
factorial n = accumulate 2 "1" ++ " = " ++ show result
  where accumulate x string = 
          if x > n then string
          else accumulate (x+1) (string ++ "*" ++ show x)
        result = product [1..n]

-- Question 3
-- Write a function that takes a number n of type Int and returns a list of prime
-- numbers that are not greater then the number n. You can use the function "rem"
-- that gives you the remainder for division.

primes :: Int -> [Int]
primes n = if n < 2 then []
           else getPrimes [2] 3
  where getPrimes xs x
            | last xs > n = init xs
            | checkPrimality xs x = getPrimes (xs ++ [x]) (x + 1)
            | otherwise = getPrimes xs (x + 1)
        checkPrimality :: [Int] -> Int -> Bool
        checkPrimality xs x = length (filter (== 0) (map (rem x) xs)) == 0
