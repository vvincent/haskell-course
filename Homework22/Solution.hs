
-- Question 1
-- Below is a version of the Tic-Tac-Toe game that does not use State and gets
-- the player choices from the user input instead of randomly generating the choices.
-- Re-write it such that you use the StateIO monad that we defined in the lesson.

-- We present here only the solution and do not copy the original code from the homework.

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-} 

import Control.Monad.Trans (MonadIO(..))
import Control.Monad (ap, liftM)
import Control.Monad.State (MonadState(get, put))

newtype StateIO s a = StateIO {runStateIO :: s -> IO (a, s)}

instance Functor (StateIO s) where
    fmap = liftM

instance Applicative (StateIO s) where
    pure = return
    (<*>) = ap

instance Monad (StateIO s) where
    return a = StateIO $ \s -> return (a, s)
    n >>= k = StateIO $ \s -> do (a, s') <- runStateIO n s
                                 runStateIO (k a) s'

instance MonadIO (StateIO s) where
    liftIO io = StateIO $ \st -> do x <- io
                                    return (x, st)

instance MonadState s (StateIO s) where
    get = StateIO $ \st -> return (st,st)
    put st = StateIO $ const $ return ((),st)

data Player = XPlayer | OPlayer deriving (Eq, Show)

data Choice = Empty | X | O
  deriving (Show, Eq)

type FieldIndex = Int

data GameState = GameState
  { currentBoard :: [Choice]
  , currentPlayer :: Player
  } deriving Show

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
    let board = currentBoard gs

    if player == XPlayer
    then putStrLn "Player X make your choice:"
    else putStrLn "Player O make your choice:"

    (gameFinished, newGS) <- runStateIO resolveTurn gs
    if gameFinished 
    then do
        printBoard newGS
        putStrLn "Game finished."
    else playGame newGS

resolveTurn :: StateIO GameState Bool
resolveTurn = do
    gs <- get
    let freeFields = getFreeFields gs
    choice <- liftIO $ getChoice freeFields
    applyMove choice
    isGameDone

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

applyMove :: Int -> StateIO GameState ()
applyMove choice = do
  gs <- get
  let player = currentPlayer gs
      board = currentBoard gs
      newBoard = if player == XPlayer
                 then [if ind /= choice then board !! (ind-1) else X | ind <- [1..9]]
                 else [if ind /= choice then board !! (ind-1) else O | ind <- [1..9]]
  let newGS = gs { currentPlayer = nextPlayer player, currentBoard = newBoard }
  liftIO $ printBoard newGS
  put newGS

nextPlayer :: Player -> Player
nextPlayer XPlayer = OPlayer
nextPlayer OPlayer = XPlayer

isGameDone :: StateIO GameState Bool
isGameDone = do
  gs <- get
  let freeFields = getFreeFields gs
  return $ length freeFields == 0

printBoard :: GameState -> IO ()
printBoard gs = do
    let board = currentBoard gs
    let stateToString st = case st of
                             Empty -> "-"
                             X -> "X"
                             O -> "O"
        printInd ind = stateToString $ board !! ind
    mapM_ putStr [printInd 0,"|", printInd 1,"|", printInd 2, "\n"]
    putStrLn "-----"
    mapM_ putStr [printInd 3,"|", printInd 4,"|", printInd 5, "\n"]
    putStrLn "-----"
    mapM_ putStr [printInd 6,"|", printInd 7,"|", printInd 8, "\n"]