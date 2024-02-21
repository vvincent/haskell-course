module TicTacToe (clearScreen, emptyBoard, gameLoop) where

import           Data.Char     (ord)
import           Data.List     (intercalate)
import           System.Random (randomRIO)

------------------------------------------- TYPES -------------------------------------------------
---------------------------------------------------------------------------------------------------

-- A board is a list of strings
-- We will use characters to represent tile states
-- - Player 1 occupies a tile with an 'x'
-- - Player 2 (or computer) occupies a tile with an 'o'
-- - Free tiles contain a space character ' '
type Board = [String]

-- 3x3 empty board (all space characters)
emptyBoard :: Board
emptyBoard = ["   ", "   ", "   "]

-- We'll use Move and Position to represent the (x,y) coordinates
-- of both moves and the position of tiles
type Move = (Int, Int)
type Position = Move

---------------------------------------- MAIN GAME LOOP -------------------------------------------
---------------------------------------------------------------------------------------------------

gameLoop :: IO () -> Board -> Char -> Bool -> IO()
gameLoop main board player singlePlayer
  -- CPU's turn in Single Player mode
  | singlePlayer && (player == 'o') = do
      cpuMove <- getCPUMove board -- Get the CPU's move
      let (newBoard,_) = doMove board cpuMove player -- Apply the move to the board
      continueGame newBoard cpuMove -- Check how the game should continue
  -- Human's turn in Single Player mode OR any player's turn in 2 Player mode
  | otherwise = do
    putStrLn "\n" >> printBoard board >> putStrLn "\n"
    putStrLn $ "Player " ++ [player] ++ ", please enter a move: "
    line <- getLine -- Get the player's move
    clearScreen
    if "exit" == line then
      return () -- If the input is 'exit', then exit the program
    else do
      let (move, isValid) = parseMove line -- Get the player's move and parse
      if isValid then do -- The move was parsed properly
        let (newBoard, moveCorrectlyApplied) = doMove board move player -- Apply the move to the board
        if moveCorrectlyApplied then do -- The move was not out of bounds or already taken
          continueGame newBoard move -- Check how the game should continue
        else do
          putStrLn "Move out of bounds or already occupied tile! \nPlease try again."
          gameLoop main board player singlePlayer -- Continue the game loop with unchanged board
      else do
        putStrLn "Invalid move.\nPlease try again." -- Parser could not determine the move
        gameLoop main board player singlePlayer -- Continue the game loop with unchanged board
  where
    -- Check if the game is over
    continueGame :: Board -> Move -> IO ()
    continueGame b m
        | isWinner b m = do
            printBoard b
            putStrLn $ "\nPlayer " ++ [player] ++ " is the winner!"
            restart main
        | isOutOfMoves b = do
            printBoard b
            putStrLn "\nTie Game!"
            restart main
        | otherwise = gameLoop main b (nextPlayer player) singlePlayer


----------------------------------- MOVE-RELATED FUNCTIONS ----------------------------------------
---------------------------------------------------------------------------------------------------

-- Get randomly generated and valid move
getCPUMove :: Board -> IO Move
getCPUMove board = do
  x <- randomRIO (0,length board - 1)
  y <- randomRIO (0,length (head board) - 1)
  if snd $ doMove board (x,y) 'o' then
    return (x,y)
  else
    getCPUMove board

-- This function will return the state of the board after applying a move.
-- If the move is invalid, the original board state will be returned and the Bool value will
-- be flagged as False indicating an invalid move.
doMove :: Board -> Move -> Char -> (Board, Bool)
doMove b (x,y) player
    | x < 0 || y < 0 || x >= w || y >= h  = (b, False) -- Out of bounds
    | getValue (x,y) b /= ' '             = (b, False) -- Location already taken
    | otherwise                           = (putValue (x,y) player b, True) -- Modify the board
  where
    w = length $ head b  -- width of board
    h = length b -- height of board


-- Parses a String (e.g., "A2") into a Move (e.g., (0,2))
-- Also returns a Bool value representing whether or not the move is valid
parseMove :: String -> (Move, Bool)
parseMove [x,y]
    | elem x ['A'..'Z'] && elem y ['0'..'9'] = ((ord x - 65, ord y - 48), True) -- Valid move
parseMove _                                  = ((0,0), False) -- Invalid move

----------------------------------- BOARD-RELATED FUNCTIONS ---------------------------------------
---------------------------------------------------------------------------------------------------

-- Prints NxN size board into the console
printBoard :: Board -> IO ()
printBoard board = do
  let finalBoard = header ++ "\n\n" ++ intercalate rowSep ( labelRowStr $ map rowStr board)
  putStrLn finalBoard
  where
    width         = length $ head board
    header        = "   " ++ intercalate "   " (strArr $ take width ['A'..]) -- Header: A,B..Z labels
    rowStr        = intercalate " | " . map (: []) -- Place vertical bars between x's and o's in a row
    labelRowStr s = [show n ++ "  " ++ x | n <- [0..length s - 1], x <- [s!!n]] -- Add 1,2..n to the front of each row
    rowSep        = "\n   " ++ tail (init $ intercalate "+" $ replicate width "---") ++ "\n" -- Generate row separators ("---+---+---")

-- Clear the terminal screen
clearScreen :: IO ()
clearScreen = putStrLn $ replicate 40 '\n'

----------------------------------- GAME-RELATED FUNCTIONS ----------------------------------------
---------------------------------------------------------------------------------------------------

-- Restart or Exit the game based on user input
restart :: IO() -> IO()
restart main = do
  putStrLn "Would you like to play again? (y/n)"
  resp <- getLine
  let continuation | resp == "y" = main
                   | resp == "n" = return ()
                   | otherwise = do
                       putStrLn "Invalid input. Please enter 'y' or 'n'"
                       restart main
  continuation

-- Check if there is a winner.
-- Since the board is only updated where the last move takes place, it is sufficient to check
-- the two diagonals and only the row and column where that move is located.
isWinner :: Board -> Move -> Bool
isWinner b (x,y) = vert || horiz || diagUpperLeft || diagUpperRight
  where
    dUL             = diagUL b -- Extract the upper left daigonal array
    dUR             = diagUR b -- Extract the upper right diaganal array
    -- Test if any of the diag, vert, or horiz contain all the same cahracter (a winner)
    vert            = allSame $ b !! y -- Column contains the same character
    horiz           = allSame $ map (!! x) b -- Row contains the same character
    diagUpperLeft   = not ( all (== ' ') dUL) && allSame dUL -- Check upper left diag
    diagUpperRight  = not ( all (== ' ') dUR) && allSame dUR -- Check upper right diag

-- Tests if the board contains no space characters
isOutOfMoves :: Board -> Bool
isOutOfMoves = not . any (elem ' ')

---------------------------------- HELPER FUNCTIONS -----------------------------------------------
---------------------------------------------------------------------------------------------------

-- Insert an item into an array
insertByIndex :: Int -> a -> [a] -> [a]
insertByIndex index newVal list = let (xs,ys) = splitAt index list in xs ++ newVal:drop 1 ys

-- Insert newVal in tile of position (x,y)
putValue :: Position -> a -> [[a]] -> [[a]]
putValue (x,y) newVal mat = insertByIndex y (insertByIndex x newVal (mat !! y)) mat

-- Get the value of a tile in position (x,y)
getValue :: Position -> [[a]]  -> a
getValue (x,y) mat = (mat !! y) !! x

-- Converts an array of characters into an array of single-length strings
strArr :: String -> [String]
strArr = map (:[])

-- Get the next player
nextPlayer :: Char -> Char
nextPlayer 'x' = 'o'
nextPlayer 'o' = 'x'
nextPlayer _   = error "Invalid character: This should be impossible"

-- Tests if all items in the list are the same
allSame :: Eq a => [a] -> Bool
allSame (x:xs) = all (== x) xs
allSame []     = error "Empty list: This should be impossible"

-- Get diagonal from upper right to lower left (assumes square array)
diagUR :: [[a]] -> [a]
diagUR xs = [(xs !! n) !! n | n <- [0..length xs - 1] ]

-- Get diagonal from upper left to lower right (assumes square array)
diagUL :: [[a]] -> [a]
diagUL xs = [ (xs !! n) !! (len - n - 1) | n <- [0..len - 1] ]
  where len = length xs
