
import System.Directory (listDirectory, doesFileExist)
import Data.Time.Clock (diffUTCTime, getCurrentTime)

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

{- Question 4
Write a program that prints the directory tree structure from the current
folder. Below you can see an example output how such a structure looks like.  
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

data DirectoryStructure = File String | Folder String [IO DirectoryStructure]

printDirectoryTree :: IO ()
printDirectoryTree = do 
    putStrLn "."
    structure <- returnStructure "."
    printStructure structure 0

returnStructure :: FilePath -> IO [IO DirectoryStructure]
returnStructure filePath = do
    contents <- listDirectory filePath
    if null contents 
    then return []
    else return $ flip map contents $ \fileName -> do
                        let newFilePath = filePath ++ "/" ++ fileName
                        isFile <- doesFileExist newFilePath
                        if isFile
                        then return $ File fileName
                        else do 
                            structure <- returnStructure newFilePath
                            return $ Folder fileName structure  

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