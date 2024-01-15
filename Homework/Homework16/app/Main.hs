module Main where

import           TicTacToe (clearScreen, emptyBoard, gameLoop)

main :: IO ()
main = do
  clearScreen -- Clear terminal screen
  putStrLn "Welcome to tic-tac-toe!\n"
  putStrLn "To enter a move, type 'LN' where L is an uppercase letter signifying the column name and N is a number 0-9 signifying the row number (e.g., \"A2\").\n"
  putStr "Type '"
  putStr "exit"
  putStrLn "' at any time to quit.\n"
  putStrLn "One or two players? (1/2): "
  choice <- getLine
  let path
        | choice == "1" = gameLoop main emptyBoard 'x' True  -- Player 1 vs AI
        | choice == "2" = gameLoop main emptyBoard 'x' False -- Player 1 vs Player 2
        | choice == "exit" = return () -- Exit game
        | otherwise = putStrLn "Invalid input. Please input a '1' or '2'" >> main
  path
