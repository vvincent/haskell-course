
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

-- Question 5
-- When you want to get the second element from a tuple you can use snd function.
-- If you have a list of tuples and would like the list of second element you can use
-- map snd functions. Write now a function that extract the third element from every
-- tuple element in a list of tuples. Do not use the map function.

getThirdElements :: [(a, b, c)] -> [c]
getThirdElements [] = []
getThirdElements ((x1,x2,x3):xs) = x3 : getThirdElements xs

-- Question 6
-- Write your own version of foldr, scanl and scanr functions. You can use pattern matching.
-- For the foldr function use such a recursive call: foldr f v xs = foldr ...

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ v [] =  v 
myFoldr f v xs = myFoldr f (f (last xs) v) $ init xs

myScanL :: (a -> a -> a) -> a -> [a] -> [a]
myScanL f x [] = [x]
myScanL f x [y] = [f x y]
myScanL f x (y:ys) = x : myScanL f (f x y) (tail ys)

myScanR :: (a -> a -> a) -> a -> [a] -> [a]
myScanR f x ys = reverse $ myScanR' f x ys
  where myScanR' f x [] = [x]
        myScanR' f x ys = x : myScanR' f (f (last ys) x) (init ys)

-- Question 7
-- Below you have defined some beer prices in bevogBeerPrices and your order list in
-- orderList + the deliveryCost. Write a function that takes in an order and calculates
-- the cost including delivery.

bevogBeerPrices :: [(String, Double)]
bevogBeerPrices =
  [("Tak", 6.00)
  ,("Kramah", 7.00)
  ,("Ond", 8.50)
  ,("Baja", 7.50)]

orderList :: [(String, Int)]
orderList = [("Tak", 5)
            ,("Kramah", 4)
            ,("Ond", 3)]

deliveryCost :: Double
deliveryCost = 8.50

beerCosts :: [(String, Int)] -> Double
beerCosts order =
  foldr ((+) . snd) deliveryCost
  . filter (\name -> fst name `elem` map fst order) $
  bevogBeerPrices

main1 :: IO ()
main1 = do
  print $ beerCosts orderList

-- Question 8
-- Write a function that takes in a integer and returns a list of all prime numbers
-- that are smaller or equal to the input number. Use recursion, filter and map for it.

primes :: Int -> [Int]
primes n = if n < 2 then []
           else getPrimes [2] 3
  where getPrimes xs x
            | last xs > n = init xs
            | checkPrimality xs x = getPrimes (xs ++ [x]) (x + 1)
            | otherwise = getPrimes xs (x + 1)
        checkPrimality :: [Int] -> Int -> Bool
        checkPrimality xs x = length (filter (== 0) (map (rem x) xs)) == 0

main2 :: IO ()
main2 = do
  print $ primes 15