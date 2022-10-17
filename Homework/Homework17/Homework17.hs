
import Text.Read (readMaybe) 

-- Question 1
-- For the Wrapper type from the previous lesson create also an instance of Applicative.
-- Then create a function that asks the user to input two numbers, creates Wrapper types
-- with them and summs them. Use the <*> operator and pure. In case one user input is not 
-- a valid number the result should be the Empty data constructor of the Wrapper type. You
-- can use the readMaybe function that we import at the beginning.   

data Wrapper a = Empty | Wrapper a deriving Show


-- Question 2
-- Write a function that takes an aritmetic operator that has an instance of Fractional: +, -, *, / 
-- and a list of type [Double] and then calulates the number of all possible computations where you 
-- can take any of two elements from the list and uses the provided operator on them. For which of 
-- the operators above is the number the smallest for the list [1..5]? Does the operator that has 
-- the smallest number change when you chage the input list?


-- Question 3
-- For the Cube type and data defined below create a Show instance that prints possible combinations of
-- the numbers and their probabilites. Create a Nums Semigroup instance that combines e.g. the strings
-- "1" and "2" to the string "1-2". Create a Semigroup and Monoid instance for Cube that combines all
-- posible cube results for 2 cubes and their probabilities into a new Cube object. Then evalueate:
-- cube1 <> cube2 and mconcat [cube1, cube1, cube1]. The result for cube1 <> cube2 should be:

-- Case: 1-1, Probability: 6.0e-2
-- Case: 1-2, Probability: 9.0e-2
-- Case: 1-3, Probability: 0.15
-- Case: 2-1, Probability: 0.14
-- Case: 2-2, Probability: 0.21
-- Case: 2-3, Probability: 0.35

newtype Nums = Num String
type Numbers = [Nums]
type Probabilities = [Double]

data Cube = Cube Numbers Probabilities

cube1 :: Cube
cube1 = Cube [Num "1", Num "2"] [0.3, 0.7]

cube2 :: Cube
cube2 = Cube [Num "1", Num "2", Num "3"] [0.2, 0.3, 0.5]