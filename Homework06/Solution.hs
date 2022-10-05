
-- Question 1
-- Write a version of the product function without using pattern matching.

myProduct :: [Int] -> Int
myProduct xs = if xs == [] then 0
               else multiply xs 
  where multiply xs = if xs == [] then 1
                      else head xs * multiply (tail xs)

-- Question 2
-- Write a function that takes in a list of integers and removes from it the smallest
-- element. If the element is occuring more then once, it should remove only the first
-- occurence of the smallest element.

-- Chalange: Write the this function in such a way that if you add some code to it and
-- not remove any, the function will be removing all instances of the smallest eleement.

removeMin :: [Int] -> [Int]
removeMin [] = []
removeMin xs = removeMin' (minimum xs) xs
  where removeMin' minEl [] = []
        removeMin' minEl ys = if head ys == minEl 
                             then tail ys
                             else head ys : removeMin' minEl (tail ys)

removeMin' :: [Int] -> [Int]
removeMin' [] = []
removeMin' xs = removeMin' (minimum xs) xs
  where removeMin' minEl [] = []
        removeMin' minEl ys = if head ys == minEl 
                             then removeMin' minEl (tail ys)
                             else head ys : removeMin' minEl (tail ys)

-- Question 3
-- Write a function that takes in an integer n, calculates the factorial n! and 
-- returns a string in the form of 1*2* ... *n = n! where n! is the actual result. 

factorial :: Int -> String
factorial n = accumulate 2 "1" ++ " = " ++ show result
  where accumulate x string = 
          if x > n then string
          else accumulate (x+1) (string ++ "*" ++ show x)
        result = product [1..n]

-- Question 4
-- Write your own version of functions zip and zipWith. Use pattern matching.

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
