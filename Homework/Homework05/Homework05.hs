
-- Question 1
-- For the expressions below try to gues what the result is for y=5 before you run any of
-- the functions. The rule for variable lookup and its priority is called lexical scope.

x = 2 :: Int

mult1, mult2, mult3 :: Int -> Int
mult1 y = x * y
mult2 y = (\x -> x * y) 3
mult3 y = (\y -> (\x -> x * y) 4) 3

-- Question 2
-- Write a function that takes in a positive number n1 and creates a list from 1 .. n1.
-- Then calulate the number n2 = 1/2/.../n1 * 10**n1 and create another list from 1 .. n2,
-- where you can round of n1 and n2 when you create the first and second lists. Sum the 
-- elements of each list and calculate the difference between sum 2 and sum 1. For which 
-- number n1 does the difference jump from a positive to a negative result? 

-- Use only 2 helper functions and function composition. Do not use any let-in and where
-- blocks. You are allowed only to use the built-in functions: head, tail, last, init and 
-- sum. They all operate on lists of numbers. Try them out to see how they work. You can
-- also help yourself with pattern matching and recursion.

