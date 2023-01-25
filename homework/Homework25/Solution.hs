
import Data.Char (isDigit)
import Control.Monad.Trans.Reader (ReaderT(runReaderT), ask) 
import Control.Monad.Trans (MonadTrans(lift))

{-
Question 1
Write a program that asks the user for some integer numbers,
checks if the input is correct and returns a list of factorials
of these numbers. Use a ReaderT transformer parameterized by a list.

HINT: Import the Control.Monad.Trans module and use the lift function.
-}

returnFactorials :: ReaderT [Integer] [] Integer
returnFactorials = do
    list <- ask
    element <- lift list
    let factorial = product [1..element]
    return factorial

main :: IO ()
main = do
    putStrLn "Input some integer numbers:"
    inp <- getLine
    let list = words inp
        check = all (all isDigit) list
    if check
    then do
        let numbers = map read list :: [Integer]
            factorials = runReaderT returnFactorials numbers
        putStrLn "The factorials of your list are:"
        print factorials
    else do
        putStrLn "Incorrect input. Try again."
        main


