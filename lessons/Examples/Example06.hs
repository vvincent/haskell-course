{-
    -- Recursion
    Recursion occurs when a thing is defined in terms of itself. So a recursive function is one that it's defined in terms of itself.
-}
-- Example of a recursive function in Haskell

-- This function calculates the sum of a list of integers using recursion.
-- The function takes a list of integers and returns their sum.
sum' :: [Int] -> Int
sum' [] = 0 -- Base case: the sum of an empty list is 0, which stops the recursion. 0 is the identity for addition.
sum' (x:xs) = x + sum' xs
{-
    sum [1,2,3,4,5] = 1 + sum [2,3,4,5]
                = 1 + (2 + sum [3,4,5])
                = 1 + (2 + (3 + sum [4,5]))
                = 1 + (2 + (3 + (4 + sum [5])))
                = 1 + (2 + (3 + (4 + (5 + sum []))))
                = 1 + (2 + (3 + (4 + (5 + 0))))
                = 15
-}

-- This function calculates the product of a list of integers using recursion.
-- The function takes a list of integers and returns their product.
product' :: [Int] -> Int
product' [] = 1 -- Base case: the product of an empty list is 1, which stops the recursion. 1 is the identity for multiplication.
product' (x:xs) = x * product' xs
{-
    product [1,2,3,4,5] = 1 * product [2,3,4,5]
                    = 1 * (2 * product [3,4,5])
                    = 1 * (2 * (3 * product [4,5]))
                    = 1 * (2 * (3 * (4 * product [5])))
                    = 1 * (2 * (3 * (4 * (5 * product []))))
                    = 1 * (2 * (3 * (4 * (5 * 1))))
                    = 120
-}

and' :: [Bool] -> Bool
and' [] = True -- Base case: the conjunction of an empty list is True, which stops the recursion. True is the identity for conjunction.
and' (x:xs) = x && and' xs
{-
    and' [True, False, True] = True && and' [False, True]
                        = True && (False && and' [True])
                        = True && (False && (True && and' []))
                        = True && (False && (True && True))
                        = False
-}

-- This function calculates the length of a list using recursion.
-- The function takes a list and returns its length.
length' :: [a] -> Int
length' [] = 0 -- Base case: the length of an empty list is 0, which stops the recursion.
length' (_:xs) = 1 + length' xs
{-
    length' [1,2,3,4,5] = 1 + length' [2,3,4,5]
                    = 1 + (1 + length' [3,4,5])
                    = 1 + (1 + (1 + length' [4,5]))
                    = 1 + (1 + (1 + (1 + length' [5])))
                    = 1 + (1 + (1 + (1 + (1 + length' []))))
                    = 5
-}

-- This function reverses a list using recursion.
-- The function takes a list and returns a new list with the elements in reverse order.
reverse' :: [a] -> [a]
reverse' [] = [] -- Base case: the reverse of an empty list is an empty list, which stops the recursion. 
reverse' (x:xs) = reverse' xs ++ [x]
{-
    reverse' [1,2,3,4,5] = reverse' [2,3,4,5] ++ [1]
                    = (reverse' [3,4,5] ++ [2]) ++ [1]
                    = ((reverse' [4,5] ++ [3]) ++ [2]) ++ [1]
                    = (((reverse' [5] ++ [4]) ++ [3]) ++ [2]) ++ [1]
                    = ((((reverse' [] ++ [5]) ++ [4]) ++ [3]) ++ [2]) ++ [1]
                    = ([5] ++ [4] ++ [3] ++ [2] ++ [1])
                    = [5,4,3,2,1]
-}

-- This function drops the first n elements from a list using recursion.
-- The function takes an integer n and a list, and returns the list with the first n elements removed.
drop' :: Int -> [a] -> [a]
drop' n xs | n <= 0 = xs -- Base case: if n is less than or equal to 0, return the original list, which stops the recursion.
drop' _ []          = [] -- Base case: if the list is empty, return an empty list, which stops the recursion.
drop' n (_:xs)      = drop' (n - 1) xs -- Recursive case: drop the first element and continue with the rest of the list.
{-
    drop' 3 [1,2,3,4,5] = drop' 2 [2,3,4,5]
                    = drop' 1 [3,4,5]
                    = drop' 0 [4,5]
                    = [4,5]

    drop' 0 [1,2,3,4,5] = [1,2,3,4,5]
    drop' 5 [1,2,3,4,5] = []
-}

-- This function takes the first n elements from a list using recursion.
-- The function takes an integer n and a list, and returns a new list with the first n elements.
take' :: Int -> [a] -> [a]
take' n xs | n <= 0 = [] -- Base case: if n is less than or equal to 0, return an empty list, which stops the recursion.
take' _ []          = [] -- Base case: if the list is empty, return an empty list, which stops the recursion.
take' n (x:xs)      = x : take' (n - 1) xs -- Recursive case: take the first element and continue with the rest of the list.
{-
    take' 3 [1,2,3,4,5] = 1 : take' 2 [2,3,4,5]
                        = 1 : (2 : take' 1 [3,4,5])
                        = 1 : (2 : (3 : take' 0 [4,5]))
                        = 1 : (2 : (3 : []))
                        = [1,2,3]

    take' 0 [1,2,3,4,5] = []
    take' 5 [1,2,3,4,5] = [1,2,3,4,5]
-}

map' :: (a -> b) -> [a] -> [b]
map' _ [] = [] -- Base case: if the list is empty, return an empty list, which stops the recursion.
map' f (x:xs) = f x : map' f xs -- Recursive case: apply f to the head and continue with the tail.
{-
    map' (+1) [1,2,3,4,5] = (+1) 1 : map' (+1) [2,3,4,5]
                        = 2 : map' (+1) [2,3,4,5]
                        = 2 : ((+1) 2 : map' (+1) [3,4,5])
                        = 2 : (3 : map' (+1) [3,4,5])
                        = 2 : (3 : ((+1) 3 : map' (+1) [4,5]))
                        = 2 : (3 : (4 : map' (+1) [4,5]))
                        = 2 : (3 : (4 : ((+1) 4 : map' (+1) [5])))
                        = 2 : (3 : (4 : (5 : map' (+1) [5])))
                        = 2 : (3 : (4 : (5 : ((+1) 5 : map' (+1) []))))
                        = 2 : (3 : (4 : (5 : (6 : map' (+1) []))))
                        = 2 : (3 : (4 : (5 : (6 : []))))
                        = [2,3,4,5,6]
-}

-- This function filters a list based on a predicate using recursion.
-- The function takes a predicate (a function that returns a Bool) and a list, and returns a new list with only the elements that satisfy the predicate.
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = [] -- Base case: if the list is empty, return an empty list, which stops the recursion.
filter' p (x:xs)
  | p x       = x : filter' p xs -- If the predicate p holds for x, include it in the result.
  | otherwise = filter' p xs -- If p x is False, skip x and continue with the rest of the list.
{-
    filter' even [1,2,3,4,5] = filter' even [2,3,4,5]
                             = 2 : filter' even [3,4,5]
                             = 2 : filter' even [4,5]
                             = 2 : (4 : filter' even [5])
                             = 2 : (4 : filter' even [])
                             = 2 : (4 : [])
                             = [2,4]
-}

-- foldr pattern
{-
    foldr pattern is a function that takes a binary function pattern, an initial value z, and a list xs.
    It applies the binary function pattern to each element of the list, starting from the right,
    and accumulates the result using the initial value z.
-}
-- foldr' is a custom implementation of the foldr function.
-- It takes a binary function f, an initial value z, and a list xs.
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ z []     = z -- Base case: if the list is empty, return the initial value z, which stops the recursion.
foldr' f z (x:xs) = f x (foldr' f z xs) -- Recursive case: apply f to the head x and the result of folding the tail xs.
{-
    -- same as: sum [1,2,3,4,5]
    foldr' (+) 0 [1,2,3,4,5] = 1 + foldr' (+) 0 [2,3,4,5]
                             = 1 + (2 + foldr' (+) 0 [3,4,5])
                             = 1 + (2 + (3 + foldr' (+) 0 [4,5]))
                             = 1 + (2 + (3 + (4 + foldr' (+) 0 [5])))
                             = 1 + (2 + (3 + (4 + (5 + foldr' (+) 0 []))))
                             = 1 + (2 + (3 + (4 + (5 + 0))))
                             = 15

    foldr' (+) 0 [1,2,3,4,5] == sum [1,2,3,4,5]
-}

-- We can use foldr to define the same functions as above in a more concise way.
sum'' :: [Int] -> Int
sum'' = foldr (+) 0 -- We partially apply foldr

-- Define product'' using foldr
product'' :: [Int] -> Int
product'' = foldr (*) 1 -- We partially apply foldr

-- Define and'' using foldr
and'' :: [Bool] -> Bool
and'' = foldr (&&) True -- We partially apply foldr

-- Define length'' using foldr
-- This function calculates the length of a list using foldr.
length'' :: [a] -> Int
length'' = foldr (\_ acc -> 1 + acc) 0 -- We partially apply foldr with a lambda function that counts elements
{-
    length'' [1,2,3,4,5] = foldr (\_ acc -> 1 + acc) 0 [1,2,3,4,5]    
                            = foldr (\_ acc -> 1 + acc) 0 [2,3,4,5] 1
                            = foldr (\_ acc -> 1 + acc) 0 [3,4,5] 2
                            = foldr (\_ acc -> 1 + acc) 0 [4,5] 3
                            = foldr (\_ acc -> 1 + acc) 0 [5] 4
                            = foldr (\_ acc -> 1 + acc) 0 [] 5
                            = 0 + 1 + 1 + 1 + 1 + 1
                            = 5                          
-}

reverse'' :: [a] -> [a]
reverse'' = foldr (\x acc -> acc ++ [x]) [] -- We partially apply
{-
    reverse'' [1,2,3,4,5] = foldr (\x acc -> acc ++ [x]) [] [1,2,3,4,5]
                         = foldr (\x acc -> acc ++ [x]) [2,3,4,5] 1
                         = foldr (\x acc -> acc ++ [x]) [3,4,5] 2
                         = foldr (\x acc -> acc ++ [x]) [4,5] 3
                         = foldr (\x acc -> acc ++ [x]) [5] 4
                         = foldr (\x acc -> acc ++ [x]) [] 5
                         = [5] ++ [4] ++ [3] ++ [2] ++ [1]
                         = [5,4,3,2,1]
-}

-- foldl pattern
{-
    foldl pattern is a function that takes a binary function pattern, an initial value z, and a list xs.
    It applies the binary function pattern to each element of the list, starting from the left,
    and accumulates the result using the initial value z.
-}
-- foldl'' is a custom implementation of the foldl function.
-- It takes a binary function f, an initial value z, and a list xs.
foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' _ z []     = z -- Base case: if the list is empty, return the initial value z, which stops the recursion.
foldl'' f z (x:xs) = foldl'' f (f z x) xs -- Recursive case: apply f to the accumulated value z and the head x, then continue with the tail xs.
{-
    -- same as: sum [1,2,3,4,5]
    foldl'' (+) 0 [1,2,3,4,5] = foldl'' (+) (0 + 1) [2,3,4,5]
                             = foldl'' (+) (1 + 2) [3,4,5]
                             = foldl'' (+) (3 + 3) [4,5]
                             = foldl'' (+) (6 + 4) [5]
                             = foldl'' (+) (10 + 5) []
                             = 15

    foldl'' (+) 0 [1,2,3,4,5] == sum [1,2,3,4,5]
-}

reverse''' :: [a] -> [a]
reverse''' = foldl'' (\acc x -> x : acc) [] -- We partially apply
{-
    reverse''' [1,2,3,4,5] = foldl'' (\acc x -> x : acc) [] [1,2,3,4,5]
                          = foldl'' (\acc x -> x : acc) (1 : []) [2,3,4,5]
                          = foldl'' (\acc x -> x : acc) (2 : 1 : []) [3,4,5]
                          = foldl'' (\acc x -> x : acc) (3 : 2 : 1 : []) [4,5]
                          = foldl'' (\acc x -> x : acc) (4 : 3 : 2 : 1 : []) [5]
                          = foldl'' (\acc x -> x : acc) (5 : 4 : 3 : 2 : 1 : []) []
                          = [5,4,3,2,1]
-}

-- foldr' 
{-
    All the functions we defined so far had the `'` at the end because they already existed in Haskell,
    and we didn't want to get a collision. But! `foldl'` is also a function that comes with Haskell, 
    and it works a little differently than `foldl`.
-}
foldl''' :: (b -> a -> b) -> b -> [a] -> b
foldl''' _ z []     = z -- Base case: if the list is empty, return the initial value z, which stops the recursion.
foldl''' f z (x:xs) = let z' = z `f` x
                      in seq z' $ foldl''' f z' xs -- seq is used to ensure that the result of the fold is evaluated before proceeding with the next element.
{-
    veryBigList = [1..1000000]
    foldl''' (+) 0 veryBigList = foldl''' (+) (0 + 1) [2..1000000]
                             = foldl''' (+) (1 + 2) [3..1000000]
                             = foldl''' (+) (3 + 3) [4..1000000]
                             = foldl''' (+) (6 + 4) [5..1000000]
                             = foldl''' (+) (10 + 5) []
                             = 500000500000

    foldl (+) 0 veryBigList = throw an error because it tries to keep the entire list in memory, which can lead to a stack overflow.
    foldr (+) 0 veryBigList = it will trow an error because it tries to keep the entire list in memory, which can lead to a stack overflow.
    foldl''' (+) 0 veryBigList = 500000500000 -- works
-}
