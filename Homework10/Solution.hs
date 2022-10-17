import System.Directory (createDirectory, removeDirectory, listDirectory)
import Data.Time.Clock

-- Question 1
-- Define an IO action that print the string "Hello World".

helloWorldPrint :: IO ()
helloWorldPrint = putStrLn "Hello World"

-- Question 2
-- Define an IO action that has as input a name from StdIn, and returns "Hello {NAME HERE}".

helloName :: IO ()
helloName = getLine >>= putStrLn . ("Hello " ++) 

-- Question 3
-- Define an IO action that count the amount of files in the current directory.

listAction :: IO Int
listAction = do x <- listDirectory "."
                let amount = length x
                return amount

-- Question 4
-- Define an IO action that calculates the time an other IO action takes. HINT: use getCurrentTime and diffUTCTime
-- from the module Data.Time.Clock

timeIO :: IO a -> IO a
timeIO a = do initTime <- getCurrentTime
              result <- a
              finalTime <- getCurrentTime
              let diff = diffUTCTime finalTime initTime
              print diff
              return result
