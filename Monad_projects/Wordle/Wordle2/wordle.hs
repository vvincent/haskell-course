
{-# LANGUAGE DeriveGeneric #-}
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
import Data.Aeson (decode, encode, FromJSON, ToJSON)
import Data.ByteString.Lazy as B (ByteString, length, readFile, putStrLn)
import Data.ByteString.Lazy.Char8 as BC (ByteString, pack, unpack)
import GHC.Generics (Generic)
import Data.Maybe (isNothing)
import System.Directory (doesFileExist)

type Animals = [T.Text]
type Generator = StdGen

data State = Fail | Success | Misplace

data GameState = GameState
  { currentAnimal :: String
  , currentGuesses :: [Text]
  } deriving (Show,Generic)

main :: IO ()
main = do
    jsonData <- returnGameData
    let gameData = if B.length jsonData > 0
                   then (decode jsonData :: Maybe GameState)
                   else (Nothing :: Maybe GameState)
    if isNothing gameData
    then do
        let filename = "animals.txt"
        animalsText <- Prelude.readFile filename
        let animals =
                join
                . map T.words
                . T.lines
                . toLower
                $ T.pack animalsText
            gen = mkStdGen 1

        Prelude.putStrLn $ "Game rules:\n" ++ 
                "The game let's you know the word lenght.\n" ++
                "You can type in any characters and as many as you want.\n" ++
                "If you provide to much characters the surplus will be discarted."
        startGame animals gen
    else
        Prelude.putStrLn "Todo"

returnGameData :: IO ByteString
returnGameData = do
  fileExists <- doesFileExist "./game_data"
  if fileExists
  then do
    B.readFile "./game_data"
  else do
    let jsonData = BC.pack ""
    return jsonData


startGame :: Animals -> Generator -> IO ()
startGame animals gen = do
  let (selected_index, gen') = randomR (0, Prelude.length animals - 1) gen
      selected_word = animals !! selected_index
  
  -- For debugging:
  -- putStrLn $ "The solution is: "
  -- print selected_word
  Prelude.putStrLn $ "The lenght of the word is: " ++ show (T.length selected_word)

  wordles <- play selected_word
  Prelude.putStrLn "Your guesses:"
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
      let i = 1 + Prelude.length xs

      Prelude.putStrLn $ "Please enter your animal " ++ show i ++ "/" ++ show attempts ++ ": "

      attemptstr <- getLine
      let attemp = toLower . strip $ T.pack attemptstr
      let (wordle, correct) = getWordle attemp selected_word

      printf "Current attempt: %d/%d\n\n" i attempts

      TIO.putStrLn wordle

      if correct
        then do
          Prelude.putStrLn "Congratulation!"
          printf "Attempts: %d/%d\n\n" i attempts
          return (wordle : xs)
        else do
          go (n - 1) (wordle : xs)

getWordle :: Text -> Text -> (Text, Bool)
getWordle attempt correct =
  let result = T.zipWith go attempt correct
      rest =
        if T.length attempt < T.length correct
          then T.pack $ replicate (T.length correct - T.length attempt) $ cshow Fail
          else mempty
      isCorrect = attempt == correct
   in (result <> rest, isCorrect)
  where
    go :: Char -> Char -> Char
    go ca cc
      | ca == cc = cshow Success
      | ca `elem` T.unpack correct = cshow Misplace
      | otherwise = cshow Fail