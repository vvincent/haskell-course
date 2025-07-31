import Data.Char

-- Create a higher-order function that takes 3 parameters: A function and the two parameters that that function takes, and
-- flips the order of the parameters.
-- For example this: `(/) 6 2` returns `3`. But this: `flip' (/) 6 2` returns `0.3333333333`
div' :: Double -> Double -> Double
div' x y = x / y

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

-- Create the `uncurry'` function that converts a curried function to a function on pairs. So this: `(+) 1 2` that returns `3` can be written as
-- `uncurry' (+) (1,2)` (with the two different arguments inside a pair).
uncurryPlus :: (Int,Int) -> Int
uncurryPlus x = fst x + snd x

-- best solution
uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (x,y) =  f x y

-- Create the `curry'` function that converts an uncurried function to a curried function. So this: `fst (1,2)` that returns `1` can be written as
-- `curry' fst 1 2` (with the tuple converted into two different arguments).
curryFst :: Int -> Int -> Int
curryFst x y = fst (x,y)

-- best solution
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y =  f (x, y)

-- Use higher-order functions, partial application, and point-free style to create a function that checks if a word has an uppercase letter.
-- Start with using just higher-order functions and build from there. 
anyCharUpperCase :: [Char] -> Bool
anyCharUpperCase = any isUpper

-- Create the `count` function that takes a team ("Red", "Blue", or "Green") and returns the amount of votes the team has inside `votes`.
countTeam :: String -> [String] -> Int
countTeam x xs = length . filter (== x) $ xs

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

-- Create a one-line function that filters `cars` by brand and then checks if there are any left.
filterCars :: String -> [(String,Int)] -> [(String,Int)]
filterCars s xs = filter (\x -> (fst x) == s && (snd x) > 0) xs

cars :: [(String,Int)]
cars = [("Toyota",0), ("Nissan",3), ("Ford",1)]

-- using patern matching
anyXCarsLeft :: String -> Bool
anyXCarsLeft x xs = any (\(_, cars) -> cars > 0) . filter (\(brand, _) -> brand == x) $ xs
