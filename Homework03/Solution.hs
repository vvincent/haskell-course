
-- Question 1
-- Write a function that takes in two numbers and returns their quotient such that it is not grater then 1.
-- Return the number as a string and in case that the divisor is 0 return a message why the division is not
-- possible. To implement this function use both guards and if-else-then statements. 

guardsAndIf :: Double -> Double -> String
guardsAndIf a b
  | a > b = if a /= 0 then show (a/b) else "a is larger but 0"
  | a < b = if b /= 0 then show (b/a) else "b is larger but 0"
  | otherwise = if a /= 0 then "1" else "a and b are both 0"

-- Question 2
-- Write a function that takes in two numbers and calculates the sum of squares for the product and quotient
-- of those numbers. Write the function such that you use a where statement inside a let-in stamenet and a
-- let-in stamenet inside a where statement. 

invertedConstructions :: Double -> Double -> Double
invertedConstructions a b = let sqrtProd = sqrt abProd where abProd = a * b
                            in sqrtProd + sqrtQuot
                            where sqrtQuot = let abQuot = a / b in sqrt abQuot

