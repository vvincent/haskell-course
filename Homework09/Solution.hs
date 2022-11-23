
-- Question 1
-- Define a data type parameterized by 3 different types. Then define a helper function
-- that converts the initial data to a Haskell triple and a function that applies three 
-- functions, each to one of the data contained in the parameterized type.
-- Create also a variable of this type that holds the name, age and sex of a person.
-- Then write a function that takes this variable, increases the age for 1 and appends
-- a message to the name of the person that he is 1 year older. 

data Triple a b c = Triple 
                      { first :: a
                      , second :: b
                      , third :: c} deriving Show

toHaskellTriple :: Triple a b c -> (a,b,c)
toHaskellTriple (Triple x1 x2 x3) = (x1,x2,x3)

applyFunctions :: (a1 -> a2) -> (b1 -> b2) -> (c1 -> c2) -> 
                  Triple a1 b1 c1 -> Triple a2 b2 c2
applyFunctions f1 f2 f3 (Triple x1 x2 x3) = Triple (f1 x1) (f2 x2) (f3 x3)

type Name = String
type Age = Int
data Sex = Male | Female deriving Show

person1 :: Triple Name Age Sex
person1 = Triple "Charles Hoskinson" 35 Male

personTriple :: (Name, Age, Sex)
personTriple = toHaskellTriple person1

updatePerson :: (String -> String) -> (Int -> Int) -> Triple Name Age Sex -> Triple Name Age Sex
updatePerson f1 f2 = applyFunctions f1 f2 id

person1' :: Triple Name Age Sex
person1' = updatePerson (++ " 1 year older") (+1) person1

-- print person1' -- returns Triple {first = "Charles Hoskinson 1 year older", second = 36, third = Male}

-- Question 2
-- Create a labyrinth data type that can have the coices left turn, right turn, split or finish. The
-- left and right turn continue the labyrinth in one direction, the split continues it in two directions
-- and finish does not continue the labyrinth. Create also a variable of this type.

data Labyrinth = LeftTurn Labyrinth | RightTurn Labyrinth | Split Labyrinth Labyrinth | Finish

labyrinth1 :: Labyrinth
labyrinth1 = Split 
               (LeftTurn 
                 (Split 
                   (RightTurn Finish) 
                   (LeftTurn 
                     (LeftTurn Finish)
                   )
                 )
               ) 
               (RightTurn 
                 (Split 
                   Finish 
                   (LeftTurn 
                     (RightTurn Finish)
                   )
                 )
               )

-- Question 3
-- For the previous data type write two functions that take in a labyrinth variable and return the
-- smallest and largest number of possible choices to finish the labyrinth. 



-- Question 4
-- Create a tree type that it is parameterized with a. The tree can have a leaf, a 
-- double branch that splits in two tree directions or a triple branch that splits in
-- three tree directions. The leaf data constructor is parameterized by a. Then create 
-- an instance of this tree type prameterized by a color type that can hold 3 colors.  

data Color = Green | Brown | Yellow deriving Eq
-- the Eq type class is derived because it is needed for the code in the next question.

data Tree a = Leaf a | DoubleBranch (Tree a) (Tree a) | 
              TripleBranch (Tree a) (Tree a) (Tree a) 

tree :: Tree Color
tree = DoubleBranch 
          (TripleBranch 
            (Leaf Brown)
            (Leaf Green)
            (Leaf Brown)
          )
          (DoubleBranch 
            (Leaf Yellow)
            (Leaf Green)
          )

-- Question 5
-- Write a function that sums the numbers of leafs of a certain color which you can
-- define as a Haskell variable. Use the code from the previous question.

seekedColor :: Color
seekedColor = Green

countColor :: Tree Color -> Int
countColor (Leaf color) = if color == seekedColor
                          then 1
                          else 0
countColor (DoubleBranch treeA treeB) = countColor treeA + countColor treeB
countColor (TripleBranch treeA treeB treeC) = countColor treeA + countColor treeB + countColor treeC

-- countColor tree -- returns 2