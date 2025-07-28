-- Question 1
-- Write a function that checks if the monthly consumption of an electrical device is bigger, equal, or smaller than the maximum allowed and
-- returns a message accordingly. 
-- The function has to take the hourly consumption of an electrical device, the hours of daily use, and the maximum monthly consumption allowed.
-- (Monthly usage = consumption (kW) * hours of daily use (h) * 30 days).
monthlyConsumption :: Double -> Double -> Double -> String
monthlyConsumption kw h max 
   | monthlyUsage == max = "Equal to maximum" 
   | monthlyUsage > max = "Bigger than maximum" 
   | monthlyUsage < max = "less than maximum" 
   | otherwise = "Bigger than maximum" 
 where days = 30
       monthlyUsage = kw * h * days

-- Question 2
-- Prelude:
-- We use the function `show :: a -> String` to transform any type into a String.
-- So `show 3` will produce `"3"` and `show (3 > 2)` will produce `"True"`.

-- In the previous function, return the excess/savings of consumption as part of the message.
monthlyConsumption' :: Double -> Double -> Double -> String
monthlyConsumption' kw h max 
   | monthlyUsage == max = "Equal to maximum" ++ " ( monthlyUsage: " ++ show monthlyUsage ++ ")"
   | monthlyUsage > max = "Bigger than maximum" ++ " ( monthlyUsage: " ++ show monthlyUsage ++ ")"
   | monthlyUsage < max = "less than maximum" ++ " ( monthlyUsage: " ++ show monthlyUsage ++ ")"
   | otherwise = "Bigger than maximum" ++ " ( monthlyUsage: " ++ show monthlyUsage ++ ")"
 where days = 30
       monthlyUsage = kw * h * days

-- Question 3
-- Write a function that showcases the advantages of using let expressions to split a big expression into smaller ones.
-- Then, share it with other students in Canvas.
-- volume of half of a sphere on top of a cylinder is call a vlad
vlad :: Double -> Double -> Double
vlad radius height = 
  let volumeOfSphere r = (4 / 3) * pi * r^3
      halfVolumeOfSphere r = (volumeOfSphere r) * 0.5
      areaOfCircle r = pi * r^2
      volumeOfCylinder r h = (areaOfCircle r) * h
  in (halfVolumeOfSphere radius) + volumeOfCylinder radius height

-- Question 4
-- Write a function that takes in two numbers and returns their quotient such that it is not greater than 1.
-- Return the number as a string, and in case the divisor is 0, return a message why the division is not
-- possible. To implement this function using both guards and if-then-else statements. 
div' :: Double -> Double -> String
div' dividend divisor
   | divisor == 0.0 = "in dividend/divisor, divisor can not be zero"
   | divisor < dividend = "in dividend/divisor, divisor should be greater than dividend to have a quotient/result less than 1"
   | otherwise = if divisor /= 0.0 then show (dividend / divisor) else "infinite value" 

-- Question 5
-- Write a function that takes in two numbers and calculates the sum of square roots for the product and quotient
-- of those numbers. Write the function such that you use a where block inside a let expression and a
-- let expression inside a where block. 
func :: Double -> Double -> Double
func a b = 
 let sqrtProduct = sqrt product where product = let v1 = a; v2 = b; in v1 * v2
     sqrtQuotient = sqrt quotient where quotient = let v1 = a; v2 = b; in v1 / v2
 in sqrtProduct + sqrtQuotient

-- best solution
invertedConstructions :: Double -> Double -> Double
invertedConstructions a b = let sqrtProd = sqrt abProd where abProd = a * b
                            in sqrtProd + sqrtQuot
                            where sqrtQuot = let abQuot = a / b in sqrt abQuot
 

