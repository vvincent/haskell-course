
module Main where

import Data.Array.Unboxed (UArray)
import Data.Array (Array, bounds, array, (!))
import Data.Array.ST (writeArray, MArray(newArray), runSTUArray)
import Control.Monad (forM_)

main :: IO ()
main = do
  print "Write code."
