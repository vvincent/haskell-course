
import Text.Read (readMaybe) 
import Control.Monad (forM_)

-- Question 1
-- For the Wrapper type from the previous lesson create also an instance of Applicative.
-- Then create a function that asks the user to input two numbers, creates Wrapper types
-- with them and summs them. Use the <*> operator and pure. In case one user input is not 
-- a valid number the result should be the Empty data constructor of the Wrapper type. You
-- can you the readMaybe function that we import at the beginning.   

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
    n1 <- getLine
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
-- Write a function that takes an aritmetic operatos that has an instance of Fractional: +, -, *, / 
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

printCombs1to5 :: IO ()
printCombs1to5 = do
    let list15 = [1..5]
    forM_ operators $ \func -> do
        print $ uniqueCombinations func list15

-- We briefly spoke about mapM_ in lesson 18. The forM_ function is same just with arguments fliped.
-- The least combinations give the operators + and -, and this stays the same no matter the list.
