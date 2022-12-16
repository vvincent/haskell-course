
-- Question 1
-- Write a function that takes in an integer n and returns a list of the 
-- first n fibonacci numbers. Use the function scanl to achieve this.

fibs :: Int -> [Int]
fibs n = let fib = 1 : scanl (+) 1 fib 
         in take n fib

test1 :: IO ()
test1 = print $ fibs 7

-- Question 2
-- TODO .. For instance create a cabal project that uses the function splitOn from
-- the module Data.List.Split. Because the package split does not come with the 
-- default Haskell installation it needs to be added to the cabal file as dependency.

-- Question 3
-- Create a cabal project with an app/ folder that contains Main.hs and a lib/
-- folder that contains Libraries.hs. In the Libraries.hs file specify a function
-- that takes an integer and returns True if it is prime and False if it is not.
-- Then in Main.hs let the program ask the user for a number and let him know
-- wheather it is prime or not by using the module defined in Libraries.hs.

-- The cabal project for this task can be found in the folder solution/.