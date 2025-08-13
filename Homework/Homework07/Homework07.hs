-- Question 1
-- Investigate the `Bounded` type class. What behaviours it provides?
-- Answer:
-- ghci> :i Bounded
-- The 'Bounded' type class provide the following behaviors or functions: minBound, maxBound

-- Question 2
-- The types Int and Word bellong to the same type classes. What is the difference
-- between them? Check maybe the maxBound and minBound parameter for both types.
-- Answer:
{-
    minBound :: Int
    -9223372036854775808
    maxBound :: Int
    9223372036854775807

    minBound :: Word
    0
    maxBound :: Word
    18446744073709551615

    The Word type is only positive number starting with 0 to 18446744073709551615, longer when compare with the positive Int type
    The Int type is positive and negative number stating with -9223372036854775808 to 9223372036854775807
-}


-- Question 3
-- Investigate the `Enum` type class. What behaviours provides?
-- Answer:
{-
    succ :: a -> a: Returns the next value in the enumeration.
    pred :: a -> a: Returns the previous value in the enumeration.

    ghci> succ 2
    3

    ghci> pred 2
    1
-}

-- Question 4
-- Add the most general type signatures possible to the functions below.
-- Then uncomment the functions and try to compile.
f1 :: (Fractional a, Show a) => a -> a -> String -> String
f1 x y z = show (x / y) ++ z

f2 :: (Eq a, Bounded a, Enum a) => a -> a
f2 x = if x == maxBound then minBound else succ x


-- Question 5
-- Investigate the numeric type classes to figure out which behaviors they provide to change between numeric types.
realToFrac' :: (Real a, Fractional b) => a -> b
realToFrac' x = fromRational (toRational x)