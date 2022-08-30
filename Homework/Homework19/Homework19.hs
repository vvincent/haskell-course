
import Text.Read (readMaybe) 

-- Question 1
-- For the Wrapper type from the previous lesson create also an instance of Applicative.
-- Then create a function that asks the user to input two numbers, creates Wrapper types
-- with them and summs them. Use the <*> operator and pure. In case one user input is not 
-- a valid number the result should be the Empty data constructor of the Wrapper type. You
-- can you the readMaybe function that we import at the beginning.   

data Wrapper a = Empty | Wrapper a deriving Show

-- Question 2
-- Write a function that takes an aritmetic operatos that has an instance of Fractional: +, -, *, / 
-- and a list of type [Double] and then calulates the number of all possible computations where you 
-- can take any of two elements from the list and uses the provided operator on them. For which of 
-- the operators above is the number the smallest for the list [1..5]? Does the operator that has 
-- the smallest number change when you chage the input list?

