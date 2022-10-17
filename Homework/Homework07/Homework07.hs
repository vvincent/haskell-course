
-- Question 1
-- Check out some of the functions that are defined in the type classes stated in the
-- section "Other important type classes" from lesson 7. What do they do?

-- Question 2
-- The types Int and Word bellong to the same type classes. What is the difference
-- between them? Check maybe the maxBound and minBound parameter for both types.

-- Question 3
-- Add type signatures to the functions below and use type variables. Then uncomment 
-- the functions and try to compile.

-- f1 x y z = show (x/y) ++ z

--f2 x y z = foldl (/) (read x) [y..z]

{- 
f3 x = if x == maxBound
          then minBound
          else succ x
-}