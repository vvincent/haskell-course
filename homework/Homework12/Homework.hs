
import Data.Array
import Data.Array.Unboxed
import Data.Time (getCurrentTime)
import Data.Time.Clock (diffUTCTime)

-- Question 1
-- Write a function that takes in an integer n and returns a list of the 
-- first n fibonacci numbers. Use the function scanl to achieve this.

fibs :: Int -> [Int]
fibs n = let fib = 1 : scanl (+) 1 fib 
         in take n fib

test1 :: IO ()
test1 = print $ fibs 7

-- Question 2
-- In homework for lesson 11 you had to write a program that times an IO action.
-- Create an Array and UArray variables that have both 100.000 elements and time 
-- how long does it take to access the last element. 

timeIO :: IO a -> IO ()
timeIO io = do 
    initTime <- getCurrentTime
    result <- io
    finalTime <- getCurrentTime
    let diff = diffUTCTime finalTime initTime
    putStrLn $ "Time used for io action is: " ++ show diff

printArrayElem :: IO ()
printArrayElem = do
    let array1 = Data.Array.listArray (1,100000) [1..100000]
    print $ array1 Data.Array.! 100000

printUArrayElem :: IO ()
printUArrayElem = do
    let array1 = Data.Array.Unboxed.array (1,100000) $ zip [1..100000] [1..100000] :: UArray Int Int
    print $ array1 Data.Array.Unboxed.! 100000

test2 :: IO ()
test2 = do
    timeIO printArrayElem
    timeIO printUArrayElem

-- Question 3
-- Create a cabal project with an app/ folder that contains Main.hs and a lib/
-- folder that contains Libraries.hs. In the Libraries.hs file specify a function
-- that takes an integer and returns True if it is prime and False if it is not.
-- Then in Main.hs let the program ask the user for a number and let him know
-- wheather it is prime or not by using the module defined in Libraries.hs.

-- The cabal project for this task can be found in the folder solution/.