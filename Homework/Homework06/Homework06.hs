import Data.List (foldl')

-- Question 1
-- Write a function called `repeat'` that takes a value and creates an infinite list with
-- the value provided as every element of the list.
--
-- >>> repeat 17
--[17,17,17,17,17,17,17,17,17...
repeat' :: a -> [a]
repeat' x = x : repeat' x

-- Question 2
-- Using the `repeat'` function and the `take` function we defined in the lesson (comes with Haskell),
-- create a function called `replicate'` that takes a number `n` and a value `x` and creates a list
-- of length `n` with `x` as the value of every element. (`n` has to be Integer.)
--
-- >>> replicate 0 True
-- []
-- >>> replicate (-1) True
-- []
-- >>> replicate 4 True
-- [True,True,True,True]
replicate' :: Int -> a -> [a]
replicate' n _ | n <= 0 = []
replicate' n x = take n . repeat' $ x


-- Question 3
-- Write a function called `concat'` that concatenates a list of lists.
--
-- >>> concat' [[1,2],[3],[4,5,6]]
-- [1,2,3,4,5,6]
concat' :: [[a]] -> [a]
concat' [[]] = []
concat' ([]:xs) = concat' xs
concat' ((x:xs):xy) = x : (concat' (xs : xy))

-- using foldr
concat''' :: [[a]] -> [a]
concat''' = foldr (++) []
-- Test example
testConcat :: [Int]
testConcat = concat''' [[1,2],[3],[4,5,6]]

-- Question 4
-- Write a function called `zip'` that takes two lists and returns a list of
-- corresponding pairs (zips them) like this:
--
-- >>> zip' [1, 2] ['a', 'b']
-- [(1,'a'),(2,'b')]
--
-- If one input list is shorter than the other, excess elements of the longer
-- list are discarded, even if one of the lists is infinite:
--
-- >>> zip' [1] ['a', 'b']
-- [(1,'a')]
-- >>> zip' [1, 2] ['a']
-- [(1,'a')]
-- >>> zip' [] [1..]
-- []
-- >>> zip' [1..] []
-- []
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys



-- Question 5
-- Create a function called `zipWith'` that generalises `zip'` by zipping with a
-- function given as the first argument, instead of a tupling function.
--
-- > zipWith' (,) xs ys == zip' xs ys
-- > zipWith' f [x1,x2,x3..] [y1,y2,y3..] == [f x1 y1, f x2 y2, f x3 y3..]
--
-- For example, `zipWith' (+)` is applied to two lists to produce the list of
-- corresponding sums:
--
-- >>> zipWith (+) [1, 2, 3] [4, 5, 6]
-- [5,7,9]
zipWith' :: (a -> a -> b) -> [a] -> [a] -> [b]
zipWith' _ [] _         = []
zipWith' _ _ []         = []
zipWith' f (x:xs) (y:ys) = f x y : (zipWith' f xs ys)



-- Question 6
-- Write a function called `takeWhile'` that takes a precate and a list and
-- returns the list up until an element that doesn't satisfy the predicate.
--
-- >>> takeWhile (< 3) [1,2,3,4,1,2,3,4]
-- [1,2]
-- >>> takeWhile (< 9) [1,2,3]
-- [1,2,3]
-- >>> takeWhile (< 0) [1,2,3]
-- []
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) 
 | f x       = x : (takeWhile' f xs)
 | otherwise = takeWhile' f xs

-- Question 7 (More difficult)
-- Write a function that takes in an integer n, calculates the factorial n! and
-- returns a string in the form of 1*2* ... *n = n! where n! is the actual result.
factorial :: Int -> String
factorial n
  | n < 0     = "Factorial is not defined for negative numbers."
  | n == 0    = "1 = 1"
  | otherwise = let result = product [1..n]
                    terms = map show [1..n]
                    terms' = map (\x -> x ++ "*") terms
                in concat terms' ++ " = " ++ show result

-- better solution
factorial' :: Int -> String
factorial' n = accumulate 2 "1" ++ " = " ++ show result
  where
    accumulate x string
      | x > n = string
      | otherwise = accumulate (x + 1) (string ++ "*" ++ show x)
    result = product [1 .. n]

{- factorial' :: Int -> String
factorial' n
  | n < 0     = "Factorial is not defined for negative numbers."
  | n == 0    = "1 = 1"
  | otherwise = let result = product [1..n]
                    terms = map show [1..n]
                    termsWithStars = init terms ++ [last terms]
                    equation = concat (map (++ "*") (init terms)) ++ last terms
                in equation ++ " = " ++ show result

factorial'' :: Int -> String
factorial'' n
  | n < 0     = "Factorial is not defined for negative numbers."
  | n == 0    = "1 = 1"
  | otherwise = let result = product [1..n]
                    terms = map show [1..n]
                    equation = intercalate "*" terms
                in equation ++ " = " ++ show result                 
-}

-- Question 8
-- Below you have defined some beer prices in bevogBeerPrices and your order list in
-- orderList + the deliveryCost. Write a function that takes in an order and calculates
-- the cost including delivery. Assume that the two lists have the beers in the same order.

bevogBeerPrices :: [(String, Double)]
bevogBeerPrices =
  [ ("Tak", 6.00),
    ("Kramah", 7.00),
    ("Ond", 8.50),
    ("Baja", 7.50)
  ]

orderList :: [(String, Double)]
orderList =
  [ ("Tak", 5),
    ("Kramah", 4),
    ("Ond", 7)
  ]

deliveryCost :: Double
deliveryCost = 8.50

cost :: [(String,Double)] -> [(String,Double)] -> Double -> Double
cost xs [] d = deliveryCost
cost [] ys d = deliveryCost
cost (x:xs) (y:ys) d = (snd x * snd y) + cost xs ys d
 
totalCost :: Double
totalCost = cost bevogBeerPrices orderList deliveryCost  

-- better solution
beerCosts :: [(String, Double)] -> Double
beerCosts = foldl' (+) deliveryCost . zipWith' (\(_, price) (_, qty)  -> price * qty) bevogBeerPrices

totalCost' :: Double
totalCost' = beerCosts orderList

