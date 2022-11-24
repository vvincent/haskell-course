
import System.Random ( mkStdGen, Random(randomR), StdGen )
import Control.Monad (forM_, when)

type BoardIndex = Int
type Connection = (BoardIndex,BoardIndex)
type ValidMove = Bool

data BoardState = Empty | P1 | P2 deriving Show
data Player = Player1 | Player2 deriving (Show, Eq)

data GameState = GameState
  { playerPositions :: [BoardIndex]
  , connectionsP1 :: [Connection]
  , connectionsP2 :: [Connection]
  , currentPlayer :: Player
  } deriving Show

initialGameState :: StdGen -> GameState
initialGameState gen = GameState
    [p1Pos, p2Pos]
    []
    []
    Player1
  where (p1Pos, gen1) = randomR (1, 25) gen
        (p2Pos, gen2) = randomR (1, 25) gen1

nextPlayer :: Player -> Player
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

main :: IO ()
main = do
    putStrLn "\nGame starting! \n\
             \Up/Down/Left/right is done with keys w/s/a/d.\n"

    let gen = mkStdGen 1
    playGame (initialGameState gen)

playGame :: GameState -> IO ()
playGame gs = do
    let player = currentPlayer gs
    if isGameFinished gs
    then do
        putStrLn "Game is finished."
        putStrLn $ show (nextPlayer player) ++ " won the game:"
    else do
        putStrLn "Current board is:"
        printBoard gs

        putStrLn $ show player ++ " make you move:"
        move <- getMove

        let (moveValid, newGs) = processMove move gs
        if moveValid
        then playGame newGs
        else do
            putStrLn "Move not valid. Try again:"
            playGame gs

isGameFinished :: GameState -> Bool
isGameFinished gs = all not moves
    where (moveRight, _) = processMove 'd' gs
          (moveLeft, _) = processMove 'a' gs
          (moveUp, _) = processMove 'w' gs
          (moveDown, _) = processMove 's' gs
          moves = [moveRight, moveLeft, moveUp, moveDown]

getMove :: IO Char
getMove = do
    move <- getLine
    if length move == 1
    then do
        let charMove = head move
        if charMove `elem` "asdw"
        then return charMove
        else do
            putStrLn "Not a valid move. Try again."
            getMove
    else do
        putStrLn "You have to input only one character."
        getMove

processMove :: Char -> GameState -> (ValidMove, GameState)
processMove move gs = (boundsCheck && freeFieldCheck && boardLimitsCheck, newGs)
    where moveFunction = case move of
                            'a' -> flip (-) 1
                            'd' -> (+1)
                            'w' -> flip (-) 5
                            's' -> (+5)
                            _ -> id
          isPlayer1 = currentPlayer gs == Player1
          currentPos = if isPlayer1
                       then playerPositions gs !! 0
                       else playerPositions gs !! 1
          newPosition = moveFunction currentPos
          boundsCheck = newPosition > 0 && newPosition < 26
          freeFieldCheck = newPosition `elem` getFreeFields gs
          boardLimitsCheck = case move of
                                'a' -> currentPos `notElem` [1,6,11,16,21]
                                'd' -> currentPos `notElem` [5,10,15,20,25]
                                'w' -> currentPos `notElem` [1,2,3,4,5]
                                's' -> currentPos `notElem` [21,22,23,24,25]
                                _ -> False
          updatedPositions = if isPlayer1
                             then [newPosition, playerPositions gs !! 1]
                             else [playerPositions gs !! 0, newPosition]
          (newConnectionsP1,newConnectionsP2) = if isPlayer1
                                                then (connectionsP1 gs ++ [sortIntTuple (currentPos, newPosition)], connectionsP2 gs)
                                                else (connectionsP1 gs, connectionsP2 gs ++ [sortIntTuple (currentPos, newPosition)])
          newGs = gs { playerPositions = updatedPositions
                     , connectionsP1 = newConnectionsP1
                     , connectionsP2 = newConnectionsP2
                     , currentPlayer = nextPlayer $ currentPlayer gs} 

getFreeFields :: GameState -> [BoardIndex]
getFreeFields gs = filter (`notElem` occupiedFields) [1..25]
    where connections = connectionsP1 gs ++ connectionsP2 gs
          tuplesToList [] = []
          tuplesToList ((x1,x2):xs) = x1 : x2 : tuplesToList xs
          occupiedFields = tuplesToList connections

sortIntTuple :: (Int, Int) -> (Int, Int)
sortIntTuple (x1, x2) = if x1 > x2 
                        then (x2, x1)
                        else (x1, x2)

printBoard :: GameState -> IO ()
printBoard gs = do
    let connections = connectionsP1 gs ++ connectionsP2 gs
        [pos1,pos2] = playerPositions gs
        verticalLines n = [char ++ " " | pos <- [1..5], let char = if (pos + n*5, pos + (n + 1)*5) `elem` connections then "|" else " "]
        dots n = [char | relPos <- [1..5], let pos = relPos + n*5, let char = if pos `elem` [pos1,pos2] then (if pos == pos1 then "1" else "2") else "*"]
        horizontalLines n = [char | pos <- [1..4], let char = if (pos + n*5, pos + 1 + n*5) `elem` connections then "─" else " "]
        horizontalConnections n = merge (dots n) (horizontalLines n)
    
    forM_ [0..3] $ \i -> do
        putStrLn $ mconcat $ horizontalConnections i
        putStrLn $ mconcat $ verticalLines i 
        when (i == 3) $ do
            putStrLn $ mconcat $ horizontalConnections 4
    
    where merge xs [] = xs
          merge [] ys = ys
          merge (x:xs) ys = x:merge ys xs

