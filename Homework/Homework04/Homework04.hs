-- Question 1
-- Lets say you have the nested values defined bellow. How would you get the value of
-- 4 by using only pattern matching in a function?

nested :: [([Int], [Int])] -> Int
--nested = [([1,2],[3,4]), ([5,6],[7,8])]
nested ((_,[_,x]):_) = x
nested _ = 0

-- Question 2
-- Write a function that takes a list of elements of any type and, if the list has 3 or more elements, it
-- removes them. Else, it does nothing. Do it two times, one with multiple function definitions and one with
-- case expressions.
func1 :: [a] -> [a]
func1 (_:_:_:xs) = xs
func1 xs = xs

func2 :: [a] -> [a]
func2 xs = case xs of (_:_:_:xs) -> xs
                      xs -> xs                      

-- Question 3
-- Create a function that takes a 3-element tuple (all of type Integer) and adds them together
func3 :: (Integer, Integer, Integer) -> Integer
func3 (a,b,c) = a + b + c

-- Question 4
-- Implement a function that returns True if a list is empty and False otherwise.
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

-- Question 5
-- Write the implementation of the tail function using pattern matching. But, instead of failing if
-- the list is empty, return an empty list.
tail' :: [a] -> [a]
tail' (x:xs) = xs
tail' [] = []

-- Question 6
-- write a case expression wrapped in a function that takes an Int and adds one if it's even. Otherwise does nothing. 
-- (Use the `even` function to check if the number is even.)
addOneIfEven :: Int -> Int
addOneIfEven n = case even n of
    True -> n + 1
    False -> n

    

