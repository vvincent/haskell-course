
-- Question 1
-- Look up the functions and types that are stated in the import statements below. Write a 
-- function that takes in an integer n and creates an Array with indexes from 1 to n and
-- values that are the factorial of the index. Then create another function that takes in 
-- an array produced by the first function and returns a UArray where the values have a
-- reversed order in comparison to the input array. Use the Data.Array.ST module that 
-- enables you to work with stateful arrays. 

import Data.Array.Unboxed (UArray)
import Data.Array (Array, bounds, array, (!))
import Data.Array.ST (writeArray, MArray(newArray), runSTUArray)
import Control.Monad (forM_)

-- Since we did not covered monads here is an example how the forM_ function can be used:
printN :: Int -> IO ()
printN n = do
  forM_ [1..n] $ \ind -> do
    print ind