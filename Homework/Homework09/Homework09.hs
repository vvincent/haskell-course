
-- Question 1
-- Below you have a data type defined that holds a persons information. If you are deriving 
-- the Ord class and use the sort function on the unsortedPersons list the persons are sorted 
-- by their name from smallest to largest first letter. 
-- Implement the Ord type class for the PersonData type such that sort function will sort the 
-- persons regarding to their age from largest to smallest.

import Data.List (sort)

type Name = String
type Age = Int
type Height = Int

newtype PersonData = PersonData (Name, Age, Height) deriving (Show, Eq)

unsortedPersons :: [PersonData]
unsortedPersons = [ PersonData ("Mark", 65, 173)
                  , PersonData ("Jimmy", 45, 182)
                  , PersonData ("Brian", 55, 178)]

-- sort unsortedPersons -- when deriving the Ord type class for PersonData type
-- [PersonData ("Brian",55,178),PersonData ("Jimmy",45,182),PersonData ("Mark",65,173)]


-- Question 2
-- Create the type "Position" that can have the values: Intern, Junior, Senior, Manager, Chief.
-- Then create the type Experience that can have the values: Programming, Managing, Leading.
-- Create a function that takes in two candidates that have a Experience value and years of experience 
-- provided as an integer. And the function should returs the position apropriate for the candidate
-- and also say which candidate has priority for employment (The higher Position gives higher
-- priority and for same positions the years of experience can be compared). Test the function on a
-- set of three candidates that have experience and years: Programming 7, Programming 8, Managing 5.

