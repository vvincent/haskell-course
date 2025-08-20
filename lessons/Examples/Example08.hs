{-
    Type Synonyms
    A type synonym is a way to create a new name for an existing type. This can make your code more readable and easier to maintain.
-}

-- Example of Type Synonyms
type Address = String
type Value = Int
type Id = String

generateTx :: Address -> Address -> Value -> Id
generateTx from to value = from ++ to ++ show value

-- Example of Type Synonyms
type Name = String
type Address = (String, Int)
type Person = (Name, Address)

bob = ("Bob Smith", ("Main St.", 555)) :: Person

-- :t bob
-- :t fst bob
-- bob :: Person
-- fst bob :: Name

{-
    Defining new types with `data`
    You can define new types using the `data` keyword. This allows you to create more complex types that can have multiple constructors.

    data PaymentMethod = Cash | Card | Cryptocurrency

    data Color = Red | Green | Blue

    data Bool = True | False      -- Real definition of Bool

    data Ordering = LT | EQ | GT  -- Real definition of Ordering
-}

-- Type Synonyms
type Name = String
type Address = (String, Int)

 -- type data with multiple constructors, deriving Show is necessary for printing
data PaymentMethod = Cash | Card | Cryptocurrency deriving (Show)
 
type Person = (Name, Address, PaymentMethod)

-- Example of a person with a payment method
-- bob2 equals to an expression of type Person
bob2 = ("Bob Smith", ("Main St.", 555), Cash) :: Person
-- bob2
-- ("Bob Smith", ("Main St.", 555), Cash)

-- add function to the person type. this function determine how a person pays
howItPays :: Person -> String
howItPays (_, _, Cash) = "Pays in cash"
howItPays (_, _, Card) = "Pays with card"
howItPays (_, _, Cryptocurrency) = "Pays with cryptocurrency"

--howItPays bob2
-- "Pays in cash"

{- 
    ********************
    data type with multiple constructors and value parameters
-}

-- define type synonyms
type Point = (Float,Float)
type Radius = Float
type Width = Float
type Height = Float
type Color = String

-- define Shape data type with multiple constructors and value parameters
data Shape
    = Circle Point Radius Color
    | Rectangle Point Width Height Color

-- add area function to Shape
area :: Shape -> Float
area (Circle _ r _) = pi * r^2
area (Rectangle _ l1 l2 _) = l1 * l2

-- add color function to Shape
color :: Shape -> String
color (Circle _ _ c) = c
color (Rectangle _ _ _ c) = c

-- add point function to Shape
point :: Shape -> (Float, Float)
point (Circle p _ _) = p
point (Rectangle p _ _ _) = p

{-
    data type with record syntax
    This allows for more readable code and easier access to fields.
-}

data Shape
  = Circle
      { position :: (Float, Float)
      , radius   :: Float
      , color    :: String
      }
  | Rectangle
      { position :: (Float, Float)
      , width    :: Float
      , height   :: Float
      , color    :: String
      }
  deriving (Show)

-- Example of using the Shape data type with record syntax. create a circle
circ = Circle { position = (1, 2), radius = 6, color = "Green" }
:t circ
circ

-- Example of using the Shape data type with record syntax. create a rectangle
rect1 = Rectangle (9, 3) 7 3 "Yellow"
:t rect1
rect1

-- Example of using the Shape data type with record syntax. create a rectangle with modified width
rect2 = rect1 {width = 12}
:t rect2
rect2

-- answer result
circ :: Shape
Circle {position = (1.0,2.0), radius = 6.0, color = "Green"}
rect1 :: Shape
Rectangle {position = (9.0,3.0), width = 7.0, height = 3.0, color = "Yellow"}
rect2 :: Shape
Rectangle {position = (9.0,3.0), width = 12.0, height = 3.0, color = "Yellow"}

-- Example of using the Shape data type with record syntax. access fields
position circ

-- Example of using the Shape data type with record syntax. access fields
color rect2

-- answer result
(1.0,2.0)

"Yellow"

-- pattern matching with record syntax. calculate area
area :: Shape -> Float
area Circle {radius=r} = pi * r^2
area Rectangle {width=w,height=h} = w * h

-- example of using the area function
area circ
area rect1