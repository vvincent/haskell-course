module Main where

import Data.Array.Unboxed (UArray)
import Data.Array (Array, bounds, array, (!))
import Data.Array.ST (writeArray, MArray(newArray), runSTUArray)
import Control.Monad (forM_)

factorial :: Int -> [Int]
factorial n = map makeFactorial [1..n]
  where makeFactorial x = product [1..x]

factorialArray :: Int -> Array Int Int
factorialArray n = array (1,n) $ zip [1 .. n] $ factorial n

updateArray :: Array Int Int -> UArray Int Int
updateArray myArray = runSTUArray $ do
  let arrayLength = snd $ bounds myArray
  updatedArray <- newArray (1,arrayLength) 0
  forM_ [1..arrayLength] $ \ind -> do
        writeArray updatedArray ind $ myArray ! (arrayLength + 1 - ind)
  return updatedArray

main :: IO ()
main = do
  putStrLn "Input a positive integer number:"
  n <- getLine
  print $ factorialArray $ read n
  print $ updateArray $ factorialArray $ read n
