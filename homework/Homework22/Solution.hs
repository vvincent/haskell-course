
import Control.Monad.Writer (censor, listens, runWriter, MonadWriter(tell), Writer)

-- Question 1
-- Re-write the selectionSort and quicksort with help of a Writer monad such that they
-- return also the sorted list beside the operations count.

type List = [Int]

instance Semigroup Int where
  (<>) = (+)

instance Monoid Int where
  mempty = 0
  mappend = (<>)

selectionSort :: List -> Writer Int List
selectionSort []   = return []
selectionSort list = do
    tell 1
    let allMins = replicate count minElem
    sortedRest <- selectionSort rest
    return $ allMins ++ sortedRest
  where minElem = minimum list
        count = length $ filter (== minElem) list
        rest = filter (/= minElem) list

quicksort :: List -> Writer Int List
quicksort []     = return []
quicksort (x:xs) = do
    tell 1
    sorted1 <- quicksort lesser 
    sorted2 <- quicksort greater
    return $ sorted1 ++ [x] ++ sorted2
  where 
    lesser  = filter (< x) xs 
    greater = filter (>= x) xs 

main1 :: IO ()
main1 = do
    let list = [3, 2, 1, 4, 1, 2, 3]
        (sorted1, count1) = runWriter $ selectionSort list
        (sorted2, count2) = runWriter $ quicksort list
        
    putStrLn $ "Operation count results for the list " ++ show list
    putStrLn $ "Selection sort: " ++ show count1 ++ ". Sorted list: " ++ show sorted1
    putStrLn $ "Quick sort: " ++ show count2 ++ ". Sorted list: " ++ show sorted2

-- Question 2
-- Write a program that asks the user for 2 integers and then computes their greatest
-- common divisor with Euclid’s algorithm. The programm should also write out the steps
-- that the algorithm is performing. Use a Writer monad to acomplis this.

myGCD :: Int -> Int -> Writer [String] Int  
myGCD a b  
    | b == 0 = do  
        tell ["Greatest common divisor is: " ++ show a]
        return a  
    | otherwise = do  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        myGCD b (a `mod` b)

main2 :: IO ()
main2 = do
    putStrLn "Input first number:"
    n1 <- (read <$> getLine) :: IO Int
    putStrLn "Input second number:"
    n2 <- (read <$> getLine) :: IO Int
    mapM_ putStrLn $ snd $ runWriter (myGCD n1 n2) 

-- Question 3
-- Rewrite the example from the lesson that sums the two ages and uses the pass function, 
-- such that it will use the censor function instead. Notice how the type signatures of
-- the function sumAge gets simplified. 

logAge :: Int -> Writer [String] Int  
logAge x = do
    tell ["Age is: " ++ show x]
    return x            
      
sumAge :: Writer [String] Int
sumAge = do  
    a <- logAge 16  
    b <- logAge 18
    tell ["Summed age is: " ++ show (a + b)]
    return (a + b)

process :: Writer [String] Int
process = do
    censor transform sumAge

transform :: [String] -> [String]
transform = map ("NOTE: " ++)

main3 :: IO ()
main3 = do
    let (age, messages) = runWriter process
    mapM_ putStrLn messages

-- Question 4
-- Rewrite the example from the lesson that uses the listen function, when reading the
-- logs from the start function such that it will use the listens function instead.

type Adding a = Writer [String] a

logMsg :: String -> Adding ()
logMsg msg = tell [msg]

add1 :: Int -> Adding Int
add1 x = do
    logMsg "Starting add1."
    let y = x + 1
    logMsg $ "Computed result: " ++ show y
    return y

start :: Int -> Adding Int
start x = do
    (n, logLines) <- listens length $ add1 x
    logMsg $ "add1 logged " ++ show logLines ++ " lines"
    return n

main4 :: IO ()
main4 = do
    print "Input an integer number:"
    n <- (read <$> getLine) :: IO Int
    let (result, logs) = runWriter $ start n
    print $ "Result is: " ++ show result
    putStrLn "Logs are: "
    mapM_ print logs

