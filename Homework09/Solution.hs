
-- Question 1
-- Define a data type parameterized by 3 different types. Then define helper functions
-- that extract the first, second and third type, convert the initial data to a triple 
-- and apply three functions, each to one of the data contained in the parameterized type. 

data Triple a b c = Triple a b c deriving Show

first :: Triple a b c -> a
first (Triple x1 x2 x3) = x1 

second :: Triple a b c -> b
second (Triple x1 x2 x3) = x2

third :: Triple a b c -> c
third (Triple x1 x2 x3) = x3

toHaskellTriple :: Triple a b c -> (a,b,c)
toHaskellTriple (Triple x1 x2 x3) = (x1,x2,x3)

applyFunctions :: (a1 -> a2) -> (b1 -> b2) -> (c1 -> c2) -> 
                  Triple a1 b1 c1 -> Triple a2 b2 c2
applyFunctions f1 f2 f3 (Triple x1 x2 x3) = Triple (f1 x1) (f2 x2) (f3 x3)

-- Question 2
-- Create a recursive data type that resembles a tree structure. The tree can have
-- either a branch that splits in two directions, either a left or right branch that
-- splits in one direction or either just a leaf. 

data Tree = Leaf | Branch Tree Tree | RightBranch Tree | LeftBranch Tree deriving Show

-- Question 3
-- Define an variable of the tree type from question 2 and a function that counts
-- the numbers of leafs in a given tree variable type. You can help yourself by 
-- deriving Show for the data type in the previous question.

tree1 :: Tree
tree1 = Branch (Branch (RightBranch Leaf) Leaf) (LeftBranch (Branch Leaf Leaf))

countLeafs :: Tree -> Int
countLeafs tree = length $ filter (("Leaf" ==) . removeBrackets) treeParts
    where treeParts = words $ show tree
          removeBrackets xs = [ x | x <- xs, x `notElem` "()"]

main1 :: IO ()
main1 = print $ countLeafs tree1

-- Question 4
-- Now create a tree type same as before just that it is parameterized with a, where 
-- a represents the type of data that leafs in addition hold. Then create an instance
-- of a tree prameterized with a data type that can either contain a String or Int.
-- If a leaf is parameterized by a String the string should only contain Int numbers. 

data Numbers = Written String | Actual Int

data TreeData a = LeafData a | BranchData (TreeData a) (TreeData a) | 
                  RightBranchData (TreeData a) | LeftBranchData (TreeData a)

tree2 :: TreeData Numbers
tree2 = BranchData 
          (BranchData 
            (RightBranchData 
              (LeafData (Written "1")) 
            )
            (LeafData (Actual 2))
          )
          (LeftBranchData 
            (BranchData 
              (LeafData (Actual 3)) 
              (LeafData (Written "4"))
            )
          )

-- Question 5
-- Write a function that sums the numbers contained in the leafs that are parameterized
-- by an actual Int. Use it on the variable from the previous question. The data types
-- in the previous question should not derive or implement an instance from Show.

countIntLeafs :: TreeData Numbers -> Int
countIntLeafs (LeafData (Written str)) = 0
countIntLeafs (LeafData (Actual num)) = num
countIntLeafs (BranchData treeA treeB) = countIntLeafs treeA + countIntLeafs treeB
countIntLeafs (RightBranchData treeA) = countIntLeafs treeA
countIntLeafs (LeftBranchData treeA) = countIntLeafs treeA

main2 :: IO ()
main2 = print $ countIntLeafs tree2