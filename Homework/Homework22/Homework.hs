
import Control.Monad.State (MonadState(put, get), runState, State)

-- Question 1
-- Below is a version of the Tic-Tac-Toe game that does not use State and gets
-- the player choices from the user input instead of randomly generating the choices.
-- Re-write it such that you use the StateIO monad that we defined in the lesson.

data Player = XPlayer | OPlayer deriving Eq
data Choice = Empty | X | O deriving Eq

data GameState = GameState
  { currentBoard :: [Choice]
  , currentPlayer :: Player
  }

main :: IO ()
main = do
    putStrLn "Board indexes are:"
    mapM_ putStrLn ["1|2|3", "-----", "4|5|6", "-----", "7|8|9"]
    let initState = GameState
                      [Empty | boardInd <- [1..9]]
                      XPlayer
    playGame initState
    
playGame :: GameState -> IO ()
playGame gs = do
    let player = currentPlayer gs
        board = currentBoard gs
        freeFields = getFreeFields gs

    if length freeFields /= 0
    then do
        if player == XPlayer
        then putStrLn "Player X make your choice:"
        else putStrLn "Player O make your choice:"

        choice <- getChoice freeFields
        let newGameState = GameState
                            (if player == XPlayer
                            then [if ind /= choice then board !! (ind-1) else X | ind <- [1..9]]
                            else [if ind /= choice then board !! (ind-1) else O | ind <- [1..9]])
                            (nextPlayer player)
        printBoard newGameState
        playGame newGameState
    else do
        putStrLn "Game is over."

getFreeFields :: GameState -> [Int]
getFreeFields gs = [ind | ind <- [0..8], board !! ind == Empty]
    where board = currentBoard gs

getChoice :: [Int] -> IO Int
getChoice freeFields = do
    choice <- (read <$> getLine) :: IO Int
    if (choice - 1) `elem` freeFields
    then return choice
    else do
        putStrLn "This is not a valid choice. Try again:"
        getChoice freeFields

nextPlayer :: Player -> Player
nextPlayer XPlayer = OPlayer
nextPlayer OPlayer = XPlayer

printBoard :: GameState -> IO ()
printBoard gs = do
    let board = currentBoard gs
        stateToString st = case st of
                             Empty -> "-"
                             X -> "X"
                             O -> "O"
        printInd ind = stateToString $ board !! ind
    mapM_ putStr [printInd 0,"|", printInd 1,"|", printInd 2, "\n"]
    putStrLn "-----"
    mapM_ putStr [printInd 3,"|", printInd 4,"|", printInd 5, "\n"]
    putStrLn "-----"
    mapM_ putStr [printInd 6,"|", printInd 7,"|", printInd 8, "\n"]


