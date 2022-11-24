
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

import qualified Data.Text as T 
import qualified Data.Text.IO as TIO
import Text.Printf (printf)
import Control.Monad (join)
import System.Random (mkStdGen, Random(randomR), StdGen)
import Data.Aeson (decode, encode, FromJSON, ToJSON)
import Data.ByteString.Lazy as B (length, readFile, writeFile)
import Data.ByteString.Lazy.Char8 as BC (ByteString, pack, unpack)
import GHC.Generics (Generic)
import Data.Maybe (fromJust, isNothing)
import System.Directory (doesFileExist, removeFile)

type Animals = [T.Text]
type Generator = StdGen

data State = Fail | Success | Misplace

data GameState = GameState
  { currentAnimal :: T.Text
  , currentGuesses :: [T.Text]
  } deriving (Show, Generic, ToJSON, FromJSON)

main :: IO ()
main = do
    Prelude.putStrLn $ "Game rules:\n" ++ 
            "The game let's you know the word lenght.\n" ++
            "You can type in any characters and as many as you want.\n" ++
            "If you provide to much characters the surplus will be discarted."
    let filename = "animals.txt"
    animalsText <- Prelude.readFile filename
    let animals =
            join
            . map T.words
            . T.lines
            . T.toLower
            $ T.pack animalsText
        gen = mkStdGen 1

    jsonData <- returnGameData
    let gameData = if B.length jsonData > 0
                   then (decode jsonData :: Maybe GameState)
                   else (Nothing :: Maybe GameState)
    if isNothing gameData
    then startGame animals gen
    else do
        Prelude.putStrLn "\nLoading saved game."
        let initState = fromJust gameData
            animal = currentAnimal initState
            wordles = currentGuesses initState

        Prelude.putStrLn $ "The lenght of the word is: " ++ show (T.length animal)
        wordles <- play animal (6 - Prelude.length wordles) wordles
        Prelude.putStrLn "Your guesses:"
        TIO.putStrLn . T.unlines . reverse $ wordles
        startGame animals (mkStdGen 2)

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

  wordles <- play selected_word 6 []
  Prelude.putStrLn "Your guesses:"
  TIO.putStrLn . T.unlines . reverse $ wordles
  startGame animals gen'

cshow :: State -> Char
cshow = \case
  Fail -> '⬜'
  Success -> '🟩'
  Misplace -> '🟨'

play :: T.Text -> Int -> [T.Text] -> IO [T.Text]
play animal attempts list = go attempts list
  where
    go :: Int -> [T.Text] -> IO [T.Text]
    go 0 xs = return xs
    go n xs = do
      let i = 1 + Prelude.length xs

      Prelude.putStrLn $ "Please enter your animal " ++ show i ++ "/6: "

      attemptstr <- getLine
      let attemp = T.toLower . T.strip $ T.pack attemptstr
          (wordle, correct) = getWordle attemp animal
          wordles = wordle : xs

      printf "Current attempt: %d/6\n\n" i

      TIO.putStrLn wordle

      if correct
      then do
        Prelude.putStrLn "Congratulation!"
        printf "Attempts: %d/%d\n\n" i attempts
        removeFile "./game_data"
        return wordles
      else do
        let gs = GameState
                  { currentAnimal = animal
                  , currentGuesses = wordles
                  }
            encodedJson = encode gs
        B.writeFile "./game_data" encodedJson
        go (n - 1) wordles

getWordle :: T.Text -> T.Text -> (T.Text, Bool)
getWordle attempt correct =
  let result = T.zipWith go attempt correct
      rest = if T.length attempt < T.length correct
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
