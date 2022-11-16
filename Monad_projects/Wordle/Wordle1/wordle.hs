
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text
  ( Text,
    pack,
    strip,
    toLower,
    unpack
  )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Printf (printf)
import Control.Monad (join)
import System.Random (mkStdGen, Random(randomR), StdGen)

type Animals = [T.Text]
type Generator = StdGen

data State = Fail | Success | Misplace

main :: IO ()
main = do
    let filename = "animals.txt"
    animalsText <- readFile filename
    let animals =
            join
            . map T.words
            . T.lines
            . toLower
            $ pack animalsText
        gen = mkStdGen 1

    putStrLn $ "Game rules:\n" ++ 
               "The game let's you know the word lenght.\n" ++
               "You can type in any characters and as many as you want.\n" ++
               "If you provide to much characters the surplus will be discarted."
    startGame animals gen

startGame :: Animals -> Generator -> IO ()
startGame animals gen = do
  let (selected_index, gen') = randomR (0, length animals - 1) gen
      selected_word = animals !! selected_index
  
  -- For debugging:
  -- putStrLn $ "The solution is: "
  -- print selected_word
  putStrLn $ "The lenght of the word is: " ++ show (T.length selected_word)

  wordles <- play selected_word
  TIO.putStrLn . T.unlines . reverse $ wordles
  startGame animals gen'

cshow :: State -> Char
cshow = \case
  Fail -> '⬜'
  Success -> '🟩'
  Misplace -> '🟨'

attempts :: Int
attempts = 6

play :: Text -> IO [Text]
play selected_word = go attempts []
  where
    go :: Int -> [Text] -> IO [Text]
    go 0 xs = return xs
    go n xs = do
      let i = 1 + length xs

      putStrLn $ "Please enter your animal " ++ show i ++ "/" ++ show attempts ++ ": "

      attemptstr <- getLine
      let attemp = toLower . strip $ pack attemptstr
      let (wordle, correct) = getWordle attemp selected_word

      printf "Current attempt: %d/%d\n\n" i attempts

      TIO.putStrLn wordle

      if correct
        then do
          putStrLn "Congratulation!"
          printf "Attempts: %d/%d\n\n" i attempts
          return (wordle : xs)
        else do
          go (n - 1) (wordle : xs)

getWordle :: Text -> Text -> (Text, Bool)
getWordle attempt correct =
  let result = T.zipWith go attempt correct
      rest =
        if T.length attempt < T.length correct
          then pack $ replicate (T.length correct - T.length attempt) $ cshow Fail
          else mempty
      isCorrect = attempt == correct
   in (result <> rest, isCorrect)
  where
    go :: Char -> Char -> Char
    go ca cc
      | ca == cc = cshow Success
      | ca `elem` unpack correct = cshow Misplace
      | otherwise = cshow Fail