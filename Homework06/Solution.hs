

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
-- Write your own version of functions zip and zipWith. Use pattern matching.
-- zip takes in two lists and returns a list of tuple pairs. zipWith works similar
-- as zip just that it also takes in a function and applies it to the pair so that
-- it returns just a list of elements with the same type as the input lists.

-- zip [1..3] [3..1]
-- returns: [(1,3),(2,2),(3,1)]

-- zipWith (+) [1,2,3] [1,2,3]
-- returns: [2,4,6]

myZip :: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip [x] [y] = [(x,y)]
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

myZipWith :: (a -> a -> a) -> [a] -> [a] -> [a]
myZipWith f [] _ = []
myZipWith f _ [] = []
myZipWith f [x] [y] = [f x y]
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys
