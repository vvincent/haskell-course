
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

-- Question 2
-- The Enum type class has the function toEnum and fromEnum that let you convert
-- user defined types into Int and vice versa. For the type MyGrades below we
-- derive Enum. Implement for this type the Eq and Ord type classes by using one
-- of the Enum functions.

data MyGrades = A | B | C deriving Enum

-- Question 3
-- Create the type "Position" that can have the values: Intern, Junior, Senior, Manager, Chief.
-- Then create the type Experience that can have the values: Programming, Managing, Leading.
-- Create a function that takes in two candidates that have a Experience value and years of experience 
-- provided as an integer. And the function should returs the position apropriate for the candidate
-- and also said which candidate has priority for employment (The higher Position gives higher
-- priority and for same positions the years of experience can be compared). Test the function on a
-- set of three candidates that have experience and years: Programming 7, Programming 8, Managing 5.

