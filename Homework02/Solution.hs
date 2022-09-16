
-- Question 1
-- Add the type signatures for the functions below and then remove the comments and try to compile.

f1 :: Double -> Double -> Double -> Double
f1 x y z = x ** (y/z)

f2 :: Double -> Double -> Double -> Double
f2 x y z = sqrt (x/y - z)

f3 :: Bool -> Bool -> [Bool]
f3 x y = [x == True] ++ [y]

f5 :: String -> String -> String -> Bool
f5 x y z = x == (y ++ z)

-- Question 2
-- Are really all variables in Haskell immutable? Try googling for the answer.

-- Not all variables are immutable. An example is the data type STUArray which stands for stateful unboxed array. 
-- But you can not just declare it as a normal variable. It can be only created inside a function and then returned 
-- as a normal unboxed array which is immutable. So there are still some restrictions in Haskell what you can do
-- with mutable data structures.  

-- Question 3
-- Why should we define type signatures of functions? How can they help you? How can they help others?

-- Type signatures place restrictions on your data and can help you to catch errors you make in your code. Also
-- you can develop your programs by first defining the type signatures of functions which sets the data transformation
-- flow. And after that you just code the details. They can also help other developers to better understand your code.

-- Question 4
-- Why should you define type signatures for variables? How can they help you?

-- In Haskell we ussually declare variables inside functions. We mostly do no use type signatures for them. But in the
-- begining when you are getting errors it is helpful to add the signatures to all of your variables because the error
-- from the compiler can become more understandable if you do this.

-- Question 5
-- Are there any functions in Haskell that let you transform one type to the other? Try googling for the answer.

-- The function show and read transform a string to a numeric type and vice versa:
v1 :: Float
v1 = 1.23

v2 :: String
v2 = show v1

v3 = read v2 :: Double -- another way of adding type signatures to variables

-- You can also convert an Double to an Int with the round function.
v4 = 3.1415 :: Double
v5 = round v4 :: Int

-- Question 6
-- Can you also define in Haskell list of lists? Did we showed any example of that? How would you access the inner
-- most elements?

-- Yes you can and yes we did. When we used the function words and lines we got a list of strings and a string is 
-- also a list of characters. Here is an example:
listOfLists :: [String]
listOfLists = ["abc","def","ghi"]

v6 :: Char
v6 = listOfLists !! 1 !! 2
