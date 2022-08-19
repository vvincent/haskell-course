
-- Question 1
-- You have a list of names defined below. If you use the sort function on the list
-- the elements are sorted by their first name. The function uses the compare function
-- from the Ord type class. Try to implement the Ord type class for the FullName type 
-- such that sort function will sort the elements regarding to their last name.

import Data.List (sort)
newtype FullName = Name (String, String) deriving (Show, Eq)

unsortedNames :: [FullName]
unsortedNames = [Name ("Mark","Knopfler"),Name ("Jimmy","Page"),Name ("Brian","May")]

-- sort unsortedNames -- without implementing the Ord type class for FullName type
-- [("Brian","May"),("Jimmy","Page"),("Mark","Knopfler")]

instance Ord FullName where
  compare (Name (first1, last1)) (Name (first2, last2)) = compare last1 last2


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
-- The Enum type class has the function toEnum and fromEnum that let you convert
-- user defined types into Int and vice versa. For the type MyGrades below we
-- derive Enum. Implement for this type the Eq and Ord type classes by using one
-- of the Enum functions.

data MyGrades = A | B | C deriving Enum

instance Eq MyGrades where
 (==) grade1 grade2 = fromEnum grade1 == fromEnum grade2

instance Ord MyGrades where
 compare grade1 grade2 = compare (fromEnum grade1) (fromEnum grade2)