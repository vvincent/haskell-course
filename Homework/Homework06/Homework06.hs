
-- Question 1
-- Write a version of the product function without using pattern matching.


-- Question 2
-- Write a function that takes in a list of integers and removes from it the smallest
-- element. If the element is occuring more then once, it should remove only the first
-- occurence of the smallest element.

-- Chalange: Write the this function in such a way that if you add some code to it and
-- not remove any, the function will be removing all instances of the smallest eleement.


-- Question 3
-- Write a function that takes in an integer n, calculates the factorial n! and 
-- returns a string in the form of 1*2* ... *n = n! where n! is the actual result. 


-- Question 4
-- Write your own version of functions zip and zipWith. Use pattern matching.

-- zip [1..3] [3..1]
-- returns: [(1,3),(2,2),(3,1)]

-- zipWith (+) [1,2,3] [1,2,3]
-- returns: [2,4,6]


-- Question 5
-- When you want to get the second element from a tuple you can use snd function.
-- If you have a list of tuples and would like the list of second element you can use
-- map snd functions. Write now a function that extract the third element from every
-- tuple element in a list of tuples. Do not use the map function.


-- Question 6
-- Write your own version of foldr, scanl and scanr functions. You can use pattern matching.
-- For the foldr function use such a recursive call: foldr f v xs = foldr ...


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


-- Question 8
-- Write a function that takes in a integer and returns a list of all prime numbers
-- that are smaller or equal to the input number. Use recursion, filter and map for it.
