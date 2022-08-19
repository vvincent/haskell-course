
-- Question 1
-- You have a list of names defined below. If you use the sort function on the list
-- the elements are sorted by their first name. The function uses the compare function
-- from the Ord type class. Try to implement the Ord type class for the FullName type 
-- such that sort function will sort the elements regarding to their last name.

import Data.List (sort)
import PrelNames (mkThisGhcModule, wordTyConKey)
newtype FullName = Name (String, String) deriving (Show, Eq)

unsortedNames :: [FullName]
unsortedNames = [Name ("Mark","Knopfler"),Name ("Jimmy","Page"),Name ("Brian","May")]

-- sort unsortedNames -- without implementing the Ord type class for FullName type
-- [("Brian","May"),("Jimmy","Page"),("Mark","Knopfler")]

instance Ord FullName where
  compare (Name (first1, last1)) (Name (first2, last2)) = compare last1 last2

-- Question 2
-- The Enum type class has the function toEnum and fromEnum that let you convert
-- user defined types into Int and vice versa. For the type MyGrades below we
-- derive Enum. Implement for this type the Eq and Ord type classes by using one
-- of the Enum functions.

data MyGrades = A | B | C deriving Enum

instance Eq MyGrades where
 (==) grade1 grade2 = fromEnum grade1 == fromEnum grade2

instance Ord MyGrades where
 compare grade1 grade2 = compare (fromEnum grade1) (fromEnum grade2)

-- Question 3
-- Create the type "Position" that can have the values: Intern, Junior, Senior, Manager, Chief.
-- Then create the type Experience that can have the values: Programming, Managing, Leading.
-- Create a function that takes in two candidates that have a Experience value and years of experience 
-- provided as an integer. And the function should returs the position apropriate for the candidate
-- and also said which candidate has priority for employment (The higher Position gives higher
-- priority and for same positions the years of experience can be compared). Test the function on a
-- set of three candidates that have experience and years: Programming 7, Programming 8, Managing 5.

data Position = Intern | Junior | Senior | Manager | Chief deriving (Show, Eq, Ord)
data Experience = Programming | Managing | Leading deriving (Show, Eq, Ord)

type WorkingYears = Int
data Candidate = CandidateData Experience WorkingYears deriving (Show, Eq, Ord)

candidate1 :: Candidate
candidate1 = CandidateData Programming 7

candidate2 :: Candidate
candidate2 = CandidateData Programming 8

candidate3 :: Candidate
candidate3 = CandidateData Managing 5

data Assesment = Assesment Position Position String deriving Show
compareCandidates :: Candidate -> Candidate -> Assesment
compareCandidates c1 c2 = Assesment p1 p2 msg 
  where p1 = assesCandidate c1
        p2 = assesCandidate c2
        msg
          | c1 < c2 = "Second candidate has priority."
          | c1 > c2 = "First candidate has priority."
          | otherwise = "Candidates have same priority"

assesCandidate :: Candidate -> Position
assesCandidate (CandidateData ex wy)
    | ex == Programming = getProgramingLevel wy
    | ex == Managing = Manager
    | ex == Leading = Chief

getProgramingLevel :: Int -> Position
getProgramingLevel wy
    | wy < 2 = Intern
    | wy < 6 = Junior
    | otherwise = Senior