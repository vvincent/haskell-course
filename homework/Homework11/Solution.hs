
import System.Directory ( listDirectory )
import Data.Time.Clock ( diffUTCTime, getCurrentTime )

-- Question 1
-- Define an IO action that count the amount of files in the current directory 
-- and prints it to the terminal.

listFiles :: IO ()
listFiles = do 
    x <- listDirectory "."
    let amount = length x
    putStrLn $ "Files and folders in current directory: " ++ show amount

-- Question 2
-- Define an IO action that calculates the time an other IO action takes. 
-- HINT: use getCurrentTime and diffUTCTime from the module Data.Time.Clock

timeIO :: IO a -> IO ()
timeIO io = do 
    initTime <- getCurrentTime
    result <- io
    finalTime <- getCurrentTime
    let diff = diffUTCTime finalTime initTime
    putStrLn $ "Time used for io action is: " ++ show diff

-- Question 3
-- Write an IO action that asks the user to type something and the program then 
-- writes the message to a file called msg.txt. After that it reads the text from
-- the msg.txt file and prints it back. Use the writeFile and readFile functions.

createMsg :: IO ()
createMsg = do
    putStrLn "Type in a message:"
    msg <- getLine
    writeFile "./msg.txt" msg
    putStrLn "Wrote message to msg.txt file."
    fileContent <- readFile "./msg.txt"
    putStrLn $ "File content is: " ++ fileContent