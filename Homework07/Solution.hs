
-- Question 1
-- Check out some of the functions that are defined in the type classes stated in the
-- section "Other important type classes" from lesson 7. What do they do?

-- For the Show type class the show function can transform a variable to a string type
-- show :: Show a => a -> String

-- For the Read type class the read function can transform a string to a number type 
-- as Int, Float, Double or another type as e.g. Bool
-- read :: Read a => String -> a

-- For the Enum type class the toEnum and fromEnum functions can convert user defined
-- data types that implement Enum from an Int and to an Int.
-- toEnum :: Enum a => Int -> a
-- fromEnum :: Enum a => a -> Int

-- For the Bounded type class the parameters maxBound and minBound give the highest
-- and smallest possible value of a certain type.
-- maxBound :: Int -- returns 9223372036854775807

-- For the foldable type class the function foldl, foldr, minimum, maximum, lenght
-- and sum are very useful that we have covered in lesson 6.

-- Question 2
-- The types Int and Word bellong to the same type classes. What is the difference
-- between them? Check maybe the maxBound and minBound parameter for both types.

-- maxBound :: Int -- returns 9223372036854775807
-- maxBound :: Word -- returns 18446744073709551615
-- minBound :: Int -- returns -9223372036854775808
-- minBound :: Word -- returns 0

-- The Word type has a 2 times higher maxBound and 0 for minBound which makes it as
-- a usigned Int type. 

-- Question 3
-- Add type signatures to the functions below and use type variables and type classes. 
-- Then uncomment the functions and try to compile.

import Data.Char ( isDigit )

f1 :: (Show a, Fractional a) => a -> a -> [Char] -> [Char]
f1 x y z = show (x/y) ++ z

f2 :: (Show a, Fractional a, Read a, Enum a) => String -> a -> a -> String
f2 x y z = if all isDigit x
           then show $ foldl (/) (read x) [y..z]
           else "Error: can not parse 1st parameter."

f3 :: (Bounded a, Enum a, Eq a) => a -> a
f3 x = if x == maxBound
       then minBound
       else succ x

-- The f3 function compiles but when giving a number as an input you have to add the 
-- type signature :: Int or :: Word to not get an "Ambiguous type variable" error.