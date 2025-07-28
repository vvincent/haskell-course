{-
    -- Pattern matching in functions
-}
specialBirthday :: Int -> String
specialBirthday 1 = "First Birthday"
specialBirthday 18 = "Your are an adult"
specialBirthday 21 = "You can legally drink"
specialBirthday 65 = "You can retired"
specialBirthday age = "Nothing special about this age " ++ show age

{-
    -- Pattern matching lists
-}
whatsInsideThisList :: [Int] -> String
whatsInsideThisList []         = "It's empty!"
whatsInsideThisList [x]        = "A single element: " ++ show x
whatsInsideThisList [x, y]     = "Two elements: " ++ show x ++ " and " ++ show y
whatsInsideThisList (x:y:z:[]) = "The list has three elements: " ++ show [x,y,z]
whatsInsideThisList (x:rest)   = "The first element is: " ++ show x ++ ", and there are quite a few more!"

firstAndThird :: [Bool] -> String
firstAndThird (x:_:z:_) = "The first and third elements are: " ++ show x ++ " and " ++ show z
firstAndThird _ = "Don't have them!"

initials :: String -> String -> String
initials "" last = "What was your name again?"
initials first "" = "What was your name again?"
initials first last = (head first): "." ++ (head last): "."

-- best practice is to use pattern matching in the function arguments
initials' :: String -> String -> String  
initials' (f:_) (l:_) = [f] ++ "." ++ [l] ++ "." 
initials' _ _ = "What was your name again?"

{-
    Pattern matching tuples
-}
firstOfThree :: (a, b, c) -> a
firstOfThree (x, _, _) = x

pairFromFour :: (a, b, c, d) -> (b, d)
pairFromFour (_, x, _, y) = (x, y)

{-
    -- Case expressions
    case <Exp> of <Pattern1> -> <Result1>
                  <Pattern2> -> <Result2>
                  <Pattern3> -> <Result3>
-}
checkForZeroes :: (Int, Int, Int) -> String
checkForZeroes tuples =
 case tuples of (0, _, _) -> "Zero in the first element"
                (_, 0, _) -> "Zero in the second element"
                (_, _, 0) -> "Zero in the third element"
                (x, y, z) -> show (x, y, z)

-- using pattern matching insted of case expressions
checkForZeroes' :: (Int, Int, Int) -> String
checkForZeroes' (0, _, _) = "The first one is a zero!"
checkForZeroes' (_, 0, _) = "The second one is a zero!"
checkForZeroes' (_, _, 0) = "The third one is a zero!"
checkForZeroes' (x, y, z)   = show (x,y,z)

-- case is useful when you want to match against a value to be used in a more complex expression
checkForZeroes'' :: (Int, Int, Int) -> String
checkForZeroes'' tuple3 = "The " ++ show tuple3 ++ " has " ++
    case tuple3 of
      (0, _, _) -> "a zero as its first element"
      (_, 0, _) -> "a zero as its second element"
      (_, _, 0) -> "a zero as its third element"
      _         -> "no zeroes!"

