
-- Question 1
-- Check out some of the functions that are defined in the type classes stated in the
-- section "Other important type classes" from lesson 7. What do they do?

-- For the Show type class the show function can represent objects as strings
-- show :: Show a => a -> String

-- For the Read type class the read function can represent strings as numbers
-- read :: Read a => String -> a

-- For the Enum type class the toEnum and fromEnum functions can convert user defined
-- data types that implement Enum from an Int and to an Int.
-- toEnum :: Enum a => Int -> a
-- fromEnum :: Enum a => a -> Int

-- For the Bounded type class the parameters maxBound and minBound give the highest
-- and smallest possible value of a certain type.
-- maxBound :: Int -- returns 9223372036854775807

-- For the foldable type class the function foldl, foldr, minimum, maximum, lenght
-- and sum are very useful. The last 4 are very intuitive.

-- foldl (-) 1 [2..4]
-- returns: 1 - 2 - 3 - 4 = -8
-- foldr (-) 1 [2..4]
-- returns: 2 - (3 - (4 - 1)) = 2

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
-- Add type signatures to the functions below and use type variables. Then uncomment 
-- the functions and try to compile.

f1 :: (Show a, Fractional a) => a -> a -> [Char] -> [Char]
f1 x y z = show (x/y) ++ z

f2 :: (Fractional b, Read b, Enum b) => String -> b -> b -> b
f2 x y z = foldl (/) (read x) [y..z]

f3 :: (Bounded a, Enum a, Eq a) => a -> a
f3 x = if x == maxBound
          then minBound
          else succ x