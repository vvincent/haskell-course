
-- Question 1
-- Add the type signatures for the functions below and then remove the comments and try to compile.
-- (Use the types presented in the lecture.)
f1 :: Double -> Double -> Double -> Double
f1 x y z = x ** (y/z)

f2 :: Double -> Double -> Double -> Double
f2 x y z = sqrt (x/y - z)

f3 :: Bool -> Bool -> [Bool]
f3 x y = [x == True] ++ [y]

f4 :: [Char] -> [Char] -> [Char] -> Bool
--f4 :: [Int] -> [Int] -> [Int] -> Bool
--f4 :: String -> String -> String -> Bool
f4 x y z = x == (y ++ z)


-- Question 2
-- Why should we define type signatures of functions? How can they help you? How can they help others?
--Signature of function describe the input and output type for the function. They help you to understand what kind of data the function expects and what it will return. They also help others to use your functions correctly without needing to read the implementation details.

-- Question 3
-- Why should you define type signatures for variables? How can they help you?
-- Type signatures for variables help to clarify the type of data that the variable holds. They can prevent errors by ensuring that the variable is used consistently with its intended type throughout the code. This can also improve code readability and maintainability.

-- Question 4
-- Are there any functions in Haskell that let you transform one type to the other? Try googling for the answer.
-- Yes, Haskell provides functions like `fromIntegral` to convert between numeric types, and `show` to convert values to strings. There are also type classes like `Num`, `Fractional`, and `Real` that allow for type conversions between different numeric types.

-- Question 5
-- Can you also define in Haskell list of lists? Did we showed any example of that? How would you access the inner
-- most elements?
f5 :: [[Int]]
f5 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
-- Yes, you can define lists of lists in Haskell. An example is `f5`, which is a list of lists of integers. You can access the innermost elements using indexing, like `f5 !! 0 !! 1` to get the second element of the first list, which would return `2`.
v :: Int
v = f5 !! 0 !! 1  -- This will give you the value 2 from the first list.