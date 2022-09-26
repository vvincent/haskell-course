

-- Question 1
-- Write your own version of the functions map, sum and filter. Use pattern matching. 

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs

mySum :: Num a => [a] -> a
mySum [x] = x
mySum (x:xs) = x + (mySum xs)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x:xs) = if f x then x : myFilter f xs
                    else myFilter f xs

-- Question 2
-- Write your own version of the functions foldl and foldr. 

myFoldL :: (a -> a -> a) -> a -> [a] -> a 
myFoldL f start [] = start
myFoldL f start xs = myFoldL f newStart $ tail xs
  where newStart = f start $ head xs

myFoldR :: (a -> a -> a) -> a -> [a] -> a
myFoldR f start [x] = f x start 
myFoldR f start xs = myFoldR f (f (last xs) start) $ init xs

-- Question 3
-- Write your own version of scanl and scanr functions. You can use pattern matching.

myScanL :: (a -> a -> a) -> a -> [a] -> [a]
myScanL f x [] = [x]
myScanL f x [y] = [f x y]
myScanL f x (y:ys) = x : myScanL f (f x y) (tail ys)

myScanR :: (a -> a -> a) -> a -> [a] -> [a]
myScanR f x ys = reverse $ myScanR' f x ys
  where myScanR' f x [] = [x]
        myScanR' f x ys = x : myScanR' f (f (last ys) x) (init ys)
