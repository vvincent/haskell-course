

-- Question 1
-- Change the code of the function mySort the we wrote in the lesson so that it
-- will work as the actual sort function from the Data.List module. 

mySort :: [Int] -> [Int]
mySort [] = []
mySort xs = minimum xs : mySort (removeMin xs)
  where removeMin ys = let minYs = minimum ys
                       in removeMin' minYs ys
        removeMin' minYs [] = []
        removeMin' minYs ys = if head ys == minYs 
                              then tail ys
                              else head ys : removeMin' minYs (tail ys)

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
