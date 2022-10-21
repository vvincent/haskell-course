
-- Question 1
-- Look up the functions and types that are stated in the import statements below. Write a 
-- function that takes in an integer n and creates an Array with indexes from 1 to n and
-- values that are the factorial of the index. Then create another function that takes in 
-- an array produced by the first function and returns a UArray where the values have a
-- reversed order in comparison to the input array. To reverse the elements use a stateful
-- array that enables you to write elements one at a time. The Data.Array.ST module enables 
-- you to work with stateful arrays. Structure your project with cabal.

import Data.Array.Unboxed (UArray)
import Data.Array (Array, bounds, array, (!))
import Data.Array.ST (writeArray, MArray(newArray), runSTUArray)
import Control.Monad (forM_)

-- The forM_ function can be used to simulate a for loop whe re-writing elements from one
-- array to the other in reverse order:
printN :: Int -> IO ()
printN n = do
  forM_ [1..n] $ \ind -> do
    print ind