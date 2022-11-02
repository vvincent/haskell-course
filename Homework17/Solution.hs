
import Text.Read (readMaybe) 

-- Question 1
-- For the Wrapper type from the previous lesson create also an instance of Applicative.
-- Then create a function that asks the user to input two numbers, creates Wrapper types
-- with them and summs them. Use the <*> operator and pure. In case one user input is not 
-- a valid number the result should be the Empty data constructor of the Wrapper type. You
-- can use the readMaybe function that we import at the beginning. 

data Wrapper a = Empty | Wrapper a deriving Show

appWrapper :: Wrapper (a -> b) -> Wrapper a -> Wrapper b
appWrapper f Empty = Empty
appWrapper Empty x = Empty
appWrapper (Wrapper f) (Wrapper n) = Wrapper (f n)

instance Functor Wrapper where
    fmap f val = (pure f) <*> val   

instance Applicative Wrapper where
    (<*>) = appWrapper
    pure val = Wrapper val

sumWrapperNums :: IO ()
sumWrapperNums = do
    putStrLn "Input first number:"
    n1 <- getLine
    putStrLn "Input second number:"
    n2 <- getLine

    let maybeNum1 = readMaybe n1 :: Maybe Double 
    let maybeNum2 = readMaybe n2 :: Maybe Double 
    
    let w1 = createWrapper maybeNum1
    let w2 = createWrapper maybeNum2
    
    print $ (+) <$> w1 <*> w2

createWrapper :: Maybe Double -> Wrapper Double
createWrapper Nothing = Empty
createWrapper (Just n) = Wrapper n

-- Question 2
-- Write a function that takes an aritmetic operator that has an instance of Fractional: +, -, *, / 
-- and a list of type [Double] and then calulates the number of all possible computations where you 
-- can take any of two elements from the list and uses the provided operator on them. For which of 
-- the operators above is the number the smallest for the list [1..5]? Does the operator that has 
-- the smallest number change when you chage the input list?

uniqueCombinations :: (Fractional a, Eq a) => (a -> a -> a) -> [a] -> Int
uniqueCombinations func myList = length $ unique allCombinations
    where allCombinations = map func myList <*> myList
          unique [] = []
          unique (x:xs) = x : unique (filter (/= x) xs)

operators :: Fractional a => [a -> a -> a]
operators = [(+), (-), (*), (/)]

printCombs1to5 :: Int -> IO ()
printCombs1to5 ind = do
    if ind < length operators
    then do
        let list15 = [1..5]
            operator = operators !! ind
        print $ uniqueCombinations operator list15
        printCombs1to5 (ind + 1)
    else 
        putStrLn "Finished."

main1 :: IO ()
main1 = do
    printCombs1to5 0

-- The least combinations give the operators + and -, and this stays the same no matter the list.

-- Question 3
-- For the Cube type and data defined below we create a Show instance that prints possible combinations
-- of the numbers and their probabilites. Create a Num Semigroup instance that combines e.g. the strings
-- "1" and "2" to the string "1-2". Create a Semigroup and Monoid instance for Cube that combines all
-- posible cube results for 2 cubes and their probabilities into a new Cube object. Then evalueate:
-- cube1 <> cube2 and mconcat [cube1, cube1, cube1]. The result for cube1 <> cube2 should be:

-- Case: 1-1, Probability: 6.0e-2
-- Case: 1-2, Probability: 9.0e-2
-- Case: 1-3, Probability: 0.15
-- Case: 2-1, Probability: 0.14
-- Case: 2-2, Probability: 0.21
-- Case: 2-3, Probability: 0.35

newtype Nums = Num String
type Numbers = [Nums]
type Probabilities = [Double]

data Cube = Cube Numbers Probabilities

showPair :: Nums -> Double -> String
showPair (Num num) prob = mconcat ["Case: ",num,", Probability: ", show prob,"\n"]

instance Show Cube where
   show (Cube nums probs) = mconcat pairs
     where pairs = zipWith showPair nums probs

instance Semigroup Cube where
  (<>) cube1 (Cube [] []) = cube1
  (<>) (Cube [] []) cube2 = cube2 
  (<>) (Cube nums1 probs1) (Cube nums2 probs2) = Cube newNums newProbs
    where newNums = combineNums nums1 nums2
          newProbs = combineProbs probs1 probs2

instance Semigroup Nums where
  (<>) (Num s1) (Num "") = Num s1
  (<>) (Num "") (Num s2) = Num s2
  (<>) (Num s1) (Num s2) = Num (s1 ++ "-" ++ s2)

combineNums :: Numbers -> Numbers -> Numbers
combineNums nums1 nums2 = (<>) <$> nums1 <*> nums2

combineProbs :: Probabilities -> Probabilities -> Probabilities
combineProbs probs1 probs2 = (*) <$> probs1 <*> probs2

instance Monoid Cube where
    mempty = Cube [] []
    mappend = (<>)

cube1 :: Cube
cube1 = Cube [Num "1", Num "2"] [0.3, 0.7]

cube2 :: Cube
cube2 = Cube [Num "1", Num "2", Num "3"] [0.2, 0.3, 0.5]

main :: IO ()
main = do
  print $ cube1 <> cube2
  print $ mconcat [cube1, cube1, cube1]
