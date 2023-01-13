
import System.Directory (listDirectory, doesFileExist)
import System.CPUTime (getCPUTime)
import Data.List

-- Question 1
-- Define an IO action that count the number of files in the current directory 
-- and prints it to the terminal.

listFiles :: IO ()
listFiles = do 
    x <- listDirectory "."
    let amount = length x
    putStrLn $ "Files and folders in current directory: " ++ show amount

-- In cryptography prime numbers, positive integers only divisible by itself and 1, play a fundamental role as 
-- they provide unbreakable mathematical structures. These structures are leverage to establish secure
-- cryptographic primitives like encryption!
--
-- But, generating primes is a computational straining problem, as we will measure in the following exercise.
-- This is because, to know whether a number is a prime number, you first need to know all the previous
-- primes, and then check that they are not a divisor of this number. So, this problem gets 
-- bigger and bigger! The goal of this exercise is to showcase different functional algorithms and their performance.
--
-- Below are three functions (primes1, primes2 and primes3) which all generate a list of prime numbers.

primes1 :: Integer -> [Integer]
primes1 m = sieve [2..m]
  where
    sieve [] = []
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

primes2 :: Integer -> [Integer]
primes2 m = sieve [2..m]
             where 
             sieve (x:xs) = x : sieve (xs \\ [x,x+x..m])
             sieve [] = []

primes3 :: Integer -> [Integer]
primes3 m = turner [2..m] 
    where
    turner []     = []
    turner (p:xs) = p : turner [x | x<-xs, x<p*p || rem x p /= 0]

-- Question 2
-- Define an IO action that calculates the time another IO action takes.
-- Use the getCPUTime :: IO Integer function to get the CPU time before and after the IO action.
-- The CPU time here is given in picoseconds (which 1/1000000000000th of a second).

timeIO :: IO a -> IO ()
timeIO io = do 
    initTime <- getCPUTime
    result <- io
    finalTime <- getCPUTime
    let diff = fromIntegral (finalTime - initTime) / 1000000000000
    putStrLn $ "Time used for the IO action is: " ++ show diff ++ " seconds"

-- Question 3
-- Write an `IO ()` action that retrieves an integer from the user via getLine.
-- Use read to parse this input and with it time the execution of retrieving the 
-- last element of the primes in `[2..m]`. 

benchmark :: IO ()
benchmark = do 
    input <- getLine
    let n = read input :: Integer
    timeAndShow primes1 n
    timeAndShpw primes2 n
    timeAndShow primes3 n
    where
        timeAndShow f n = timeIO . print . show . last $ f n  

-- Question 4
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

{- Question 5 

Advanced example for students aiming at Plutus.
Write a program that prints the directory tree structure from the current
folder (denoted by "."). Below you can see an example output how such a structure looks like.  
.
├── foo1.hs
├── foo2.hs
├── bar1
    ├── bar2
    ├── foo3.hs
    ├── foo4.hs
    └── bar3
        └── foo5.hs
└── bar5
    ├── bar6
    ├── foo6.hs
    └── foo7.hs
HINT: From the System.Directory module you can use the function doesFileExist
which takes in a file path (String) and returns True if the item is a file. 
-}

-- This is a tree like data structure which will represent the structure of "."
data DirectoryStructure = File String | Folder String [IO DirectoryStructure]

-- This is a function that prints the structure neatly and use the helper functions below
printDirectoryTree :: IO ()
printDirectoryTree = do 
    putStrLn "."
    structure <- returnStructure "."
    printStructure structure 0

-- This function returns the nested directory structure by recursivly traversing the files/folders
returnStructure :: FilePath -> IO [IO DirectoryStructure]
returnStructure filePath = do
    contents <- listDirectory filePath
    return $ map go contents
    where
      go fileName = do let newFilePath = filePath ++ "/" ++ fileName
                       isFile <- doesFileExist newFilePath
                       if isFile
                       then return $ File fileName
                       else do 
                            structure <- returnStructure newFilePath
                            return $ Folder fileName structure 

-- This function converts the structure of the above function and an int to 
printStructure :: [IO DirectoryStructure] -> Int -> IO ()
printStructure [] _ = return ()
printStructure (x:xs) level = do
    structure <- x
    case structure of
        File name -> do
            printSpaces level
            printElement xs name
        Folder name ioList -> do
            printSpaces level
            printElement xs name
            printStructure ioList (level + 1)
    printStructure xs level

printElement :: [IO DirectoryStructure] -> String -> IO ()
printElement xs name = if null xs
                       then putStrLn $ "└── " ++ name
                       else putStrLn $ "├── " ++ name

printSpaces :: Int -> IO ()
printSpaces n = if n < 1 
                then return ()
                else do
                    putStr "    "
                    printSpaces (n - 1)