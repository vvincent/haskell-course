
import Control.Monad.Writer (censor, listens, runWriter, MonadWriter(tell), Writer)

-- Question 1
-- Rewrite the example from the lesson that uses the pass function, such that it 
-- will use the censor function instead. Look at how the type signatures are chaning. 

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

main1 :: IO ()
main1 = do
    let (age, messages) = runWriter process
    mapM_ putStrLn messages

-- Question 2
-- Rewrite the example from the lesson that uses the listen function, such that it 
-- will use the listens function instead.

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

main2 :: IO ()
main2 = do
    print "Input an integer number:"
    n <- (read <$> getLine) :: IO Int
    let (result, logs) = runWriter $ start n
    print $ "Result is: " ++ show result
    putStrLn "Logs are: "
    mapM_ print logs

-- Question 3
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

main3 :: IO ()
main3 = do
    putStrLn "Input first number:"
    n1 <- (read <$> getLine) :: IO Int
    putStrLn "Input second number:"
    n2 <- (read <$> getLine) :: IO Int
    mapM_ putStrLn $ snd $ runWriter (myGCD n1 n2) 
