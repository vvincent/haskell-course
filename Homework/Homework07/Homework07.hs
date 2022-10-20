
-- Question 1
-- Check out some of the functions that are defined in the type classes stated in the
-- section "Other important type classes" from lesson 7. What do they do?


-- Question 2
-- The types Int and Word bellong to the same type classes. What is the difference
-- between them? Check maybe the maxBound and minBound parameter for both types.


-- Question 3
-- Add type signatures to the functions below and use type variables and type classes. 
-- Then uncomment the functions and try to compile.

import Data.Char ( isDigit )

f1 x y z = show (x/y) ++ z

f2 x y z = if all isDigit x
           then show $ foldl (/) (read x) [y..z]
           else "Error: can not parse 1st parameter."

f3 x = if x == maxBound
       then minBound
       else succ x

-- The f3 function compiles but when giving a number as an input you have to add the 
-- type signature :: Int or :: Word to not get an "Ambiguous type variable" error.