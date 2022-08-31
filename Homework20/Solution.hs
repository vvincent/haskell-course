
import Control.Monad (forM)
import Data.Maybe (fromJust)
import Data.List (elemIndex)

-- Question 1
-- Write a function that takes in n of type Int and returns a list of type [Int]. The elements of the list 
-- are combination counts for lists [1 .. x] where the fisrt element is the count for the list [1], the second
-- for the list [1,2] and the last for the list [1..n]. How to compute a combination count for a list: e.g. the 
-- list [1,2] has 4 possible combinations which are (1,1) (1,2) (2,1) and (2,2). Do not use your knowledge of
-- mathematics. Do it by computing all combination pairs and counting them. If the user inputs a negative
-- number return a Nothing value. In your code try to simulate a for loop with the funtion forM. 
-- Additional challange: Try to write your code in a single function and make it as short as possible.

combinationCount :: Int -> Maybe [Int]
combinationCount n = do
    if n < 0 then Nothing else
      let
        allCombinations list = (\x y -> (x,y)) <$> list <*> list
        combCountList = forM [1..n] $ \i -> do
                          let combCount = length $ allCombinations [1..i]
                          return combCount
      in combCountList

-- Question 2
-- If you succesfully computed the function from the previous example you should get for n = 5 the list
-- [1,4,9,16,25] which clearly represents the function f(x) = x**2. Write now a function that uses the
-- fittingFunc defined below and finds the best exponent a (which should be the closest to 2) from the
-- provided input list of type [Double] that fits the data given your the function from the previous 
-- question. So for instance for [1.5, 1.6 .. 2.5] it should return 2.0.

fittingFunc :: Double -> Double -> Double
fittingFunc a x = x ** a

findExponent :: [Double] -> Double
findExponent candicates = candicates !! fromJust indexPosition
    where corelations = do
              tmpExponent <- candicates
              let function = fittingFunc tmpExponent
              let fittingData = map function [1..10]
              let actualData = map fromIntegral (fromJust $ combinationCount 10) :: [Double]
              let differences = zipWith (-) fittingData actualData
              let corelation = sqrt $ sum $ map (**2) differences
              return corelation
          bestCorelation = minimum $ map abs corelations
          indexPosition = elemIndex bestCorelation corelations