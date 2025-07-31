{-
    -- Higher-order functions
    A higher-order function is a function that takes other functions as arguments or returns a function as a result.
-}
-- function that takes other functions as arguments
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

complexFunc1 :: Int -> Int
complexFunc1 x = x + 1

func1 :: Int -> Int
func1 x = applyTwice complexFunc1 x

{-
    Filter functions
    A filter function is a higher-order function that takes a predicate and a list,
    and returns a new list containing only the elements that satisfy the predicate.

    filter :: forall a. (a -> Bool) -> [a] -> [a]
-}
func2 = filter even [1..20]

fruitWithA = filter tempFunct ["Apple", "Banana", "Pear", "Grape", "Wood"]
                where tempFunct x = 'a' `elem` x

{-
    any function
    The `any` function is a higher-order function that takes a predicate and a list,
    and returns `True` if any element in the list satisfies the predicate, and `False` otherwise.

    any :: forall a. (a -> Bool) -> [a] -> Bool
-}
biggerThan4 :: Int -> Bool
biggerThan4 x = x > 4

func3 :: Bool
func3 = any biggerThan4 [1,2,3,4] 

cars = [("Toyota",0), ("Nissan",3), ("Ford",1)]

biggerThan0 (_,x) = x > 0

func4 :: Bool
func4 = any biggerThan0 cars

{-
    lambda functions
    A lambda function is an anonymous function that can be defined in place without a name.
    Lambda functions are often used as arguments to higher-order functions.
    Lambda functions can be defined using the syntax: \x -> expression
    where x is the argument and expression is the body of the function.

    Predicate functions
    A predicate function is a function that takes an argument and returns a Boolean value.
-}
func5 :: Bool
func5 = any (\x -> x > 4) [1,2,3,4]

func6 :: [String]
func6 = filter (\x -> 'a' `elem` x) ["Apple", "Banana", "Pear", "Grape", "Wood"]

func7 :: Int
func7 = (\x -> x*2 + 1) 3

{-
    Precedence and associativity
    In Haskell, operators have a precedence level that determines the order in which they are evaluated.
    Operators with higher precedence are evaluated before operators with lower precedence.
    The associativity of an operator determines the order in which operators of the same precedence are evaluated. 
    Haskell has three types of associativity: left, right, and non-associative.
    The `infixl` and `infixr` keywords are used to define the precedence and associativity of operators.
    For example, the `+` operator is defined as `infixl 6 +`, which means it has a precedence of 6 and is left associative.
    The `*` operator is defined as `infixl 7 *`, which means it has a precedence of 7 and is left associative.

    -- Precedence
    :i (+)  -- infixl 6 +
    :i (*)  -- infixl 7 *
    1 + 2 * 3  -- Same as 1 + (2 * 3)   

    -- Associativity
    1 + 2 + 3 + 4  -- infixl: Same as ((1 + 2) + 3) + 4
    1 : 2 : 3 : [] -- infixr: Same as 1 : (2 : (3 : []))
    True == (False == False) -- infix: If you remove parenthesis, you'll get an error.  
    :i (**) -- infixr 8 **

    2**3**4  -- infixr: Same as 2 ** (3 ** 4)
    (2**3)**4
-}

{-
    we can define precedence and associativity when creating our own operator. Like this:
    x +++ y = x + y -- Creating +++ operator
    infixl 7 +++    -- Setting fixity of operator

    1 +++ 2 * 3  -- 9
-}
(+++) :: Num a => a -> a -> a
infixl 7 +++
x +++ y = x + y

{-
    Curried functions
    In Haskell, all functions are curried by default. This means that a function that takes multiple arguments is actually a series of functions that each take one argument.
    For example, a function that takes two arguments can be thought of as a function that takes one argument and returns another function that takes the second argument.
    This allows for partial application, where you can apply a function to some of its arguments and get back a new function that takes the remaining arguments.
-}
-- Example of a curried function
add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z

-- same as, function application (the "whitespace operator") 
-- always has the highest precedence and associates to the left, so if we make that obvious, we get:
add3' :: Int -> Int -> Int -> Int
((add3' x) y) z = x + y + z

-- :i (->)  -- infixr -1 ->

-- Curried function with explicit type signature 

-- the signature `->` is right-associative, so we can write it like this:
-- aplication (the "whitespace operator") is left-associative, so we can write it like this:   
add3'' :: Int -> (Int -> (Int -> Int)) -- Same as: add3 :: Int -> Int -> Int -> Int
((add3'' x) y) z = x + y + z           -- Same as: add3 x y z = x + y + z

-- with lambda functions
add3''' :: Int -> (Int -> (Int -> Int))
(add3''' x) y = \z -> x + y + z

-- lambda functions, `->` is right-associative
add3'''' :: Int -> (Int -> (Int -> Int))
add3'''' x = \y -> (\z -> x + y + z)

-- lambda functions, `->` is right-associative
add3''''' :: Int -> (Int -> (Int -> Int))
add3''''' = \x -> (\y -> (\z -> x + y + z))

-- And because `->` is right-associative,
-- we can remove the use-less parentheses of both the signature and definition to get a cleaner code:
add3_ :: Int -> Int -> Int -> Int
add3_ = \x -> \y -> \z -> x + y + z

{-
    Partial function application
    Partial function application is the process of applying a function to some of its arguments and getting back a new function that takes the remaining arguments.
    This is possible because Haskell functions are curried by default.
-}
createEmail :: String -> String -> String -> String
createEmail domain name lastName = name ++ "." ++ lastName ++ "@" ++ domain

createEmailTeckel :: String -> String -> String
createEmailTeckel = createEmail "teckel-owners.com" -- Partial application of createEmail with a fixed domain

createEmailSCL :: String -> String -> String
createEmailSCL = createEmail "secret-cardano-lovers.com" -- Partial application of createEmail with a fixed domain

--createEmailTeckel "Vlad" "Vincent"
-- output "Vlad.Vincent@teckel-owners.com"

-- anorher example of partial application
createEmailVlad :: String -> String -> String
createEmailVlad lastName domain = createEmail domain "Vlad" lastName

{-
    Another example of partial application
    (++ "ing") "Think"     -- Same as \x -> x ++ "ing"
    ("Anti" ++) "library"  -- Same as \x -> "Anti" ++ x    

    In the function we pass as a parameter, we need to compare if the input is larger than `4`.
    And the `>` operator is already a function that takes two parameters and compares
    if the first is larger than the second. So we can partially apply the parameter on the right
    to get the same result:
    any (\x -> x > 4) [1,2,3,4] -- same as any (> 4) [1,2,3,4]
-}

{-
    Applying and composing functions
    In Haskell, we can apply functions to their arguments using the application operator (whitespace).
    We can also compose functions using the (.) operator, which takes two functions and returns a new function that is the composition of the two functions.
    The composition operator (.) has a higher precedence than the application operator, so we can use parentheses to control the order of evaluation.
    For example, we can write:
    (f . g) x  -- Same as f (g x)
    where f and g are functions and x is an argument.
-}

{-
    The function application `$` operator
    The `$` operator is a special operator in Haskell that allows us to apply a function to an argument without using parentheses.
    It has the lowest precedence of all operators, so it allows us to avoid parentheses in many cases.
    The `$` operator is defined as:
    ($) :: (a -> b) -> a -> b
    f $ x = f x

    f g h x      = ((f g) h) x --whitespace operator as the application operator has the highest precedence and associates to the left
    f $ g $ h x  =  f (g (h x))

    (2 *) 3 + 4    -- Same as: ((2 *) 3) + 4
    (2 *) $ 3 + 4  -- Same as: (2 *) (3 + 4)

    max 5 4 + 2    -- Same as: ((max 5) 4) + 2
    max 5 $ 4 + 2  -- Same as: (max 5) (4 + 2)

    -- All these expressions are equivalent:
    show ((2**) (max 3 (2 + 2)))
    show $ (2**) (max 3 (2 + 2))
    show $ (2**) $ max 3 (2 + 2)
    show $ (2**) $ max 3 $ 2 + 2
-}

{-
    Function composition
    Function composition is the process of combining two or more functions to create a new function.
    (.)  :: (b -> c) -> (a -> b) -> a -> c
    f . g = \x -> f (g x)
    infixr 9 .
-}

complicatedF :: [Int] -> Bool
complicatedF x = any even (filter (>25) (tail ( take 10 x)))

-- The above function can be simplified using function composition:
complicatedF' :: [Int] -> Bool
complicatedF' x = any even . filter (>25) . tail . take 10 $ x

--show ((2**) (max 3 (2 + 2)))
--show . (2**) . max 3 $ 2 + 2

{-
    Point-free style
    Point-free style is a way of defining functions without explicitly mentioning their arguments.
    It is a style of programming that emphasizes the composition of functions rather than the application of functions.
-}

fourOrLarger :: Int -> Int
fourOrLarger x = max 4 x

add1 :: Int -> Int
add1 x = 1 + x

-- Point-free style
fourOrLarger' :: Int -> Int
fourOrLarger' = max 4

-- Point-free style with partial application
add1' :: Int -> Int
add1' = (1+)

-- Point-free style with function composition
complicatedF'' :: [Int] -> Bool
complicatedF'' = any even . filter (>25) . tail . take 10