
import Data.List ( isPrefixOf ) 
import GHC (TransForm(ThenForm))
import qualified Llvm as Int

-- Question 1
-- Create a recursive data type that resembles a tree structure. The tree can have
-- either a branch that splits in two directions, either a left or right branch that
-- splits in one direction or either just a leaf. 

data Tree = Leaf | Branch Tree Tree | RightBranch Tree | LeftBranch Tree deriving Show

-- Question 2
-- Define an variable of the tree type from question 1 and a function that counts
-- the numbers of leafs in a given tree variable type.

tree1 :: Tree
tree1 = Branch (Branch (RightBranch Leaf) Leaf) (LeftBranch (Branch Leaf Leaf))

countLeafs :: Tree -> Int
countLeafs tree = length $ filter ("Leaf" `isPrefixOf`) treeParts
    where treeParts = words $ show tree

main :: IO ()
main = print $ countLeafs tree1

-- Question 3
-- Now create a tree type same as before just that it is parameterized with a, where 
-- a represents the type of data that leafs in addition hold. Then create an instance
-- of a tree prameterized with a data type that can either contain a String or Int.
-- If a leaf is parameterized by a String the string should only contain Int numbers. 



-- Question 4
-- Write now a function that counts the number of leafs in a tree parameterized by
-- an actual Int. Use it on the variable from the previous question. 

