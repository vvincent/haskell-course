
-- This statement is a language pragma about which you will learn in lesson 11. We use it
-- here so we can have same function names in record syntax definitions of different types.

-- Question 1
-- Algebraic data types are types combined of other types. You have 2 choices for combining.

-- The first one is called product types where you combine types with a logical "and":
-- type Address = String
-- type Number = Int
-- data productType = Address Number

-- The second one is called sum types where you combine types with a logical "or":
-- type sumType = Descriptive Address | Numeric Number

-- Below you have two data types defined. Try to make a general type, where you extract the 
-- common properties of both and then use it when you define those two types as product types.
-- Write then a general Instruments sum type that allows the user to define same data as in the
-- initial instrument data types. Try to find more then one solution.

data Guitar = Guitar { brand :: String
                     , price :: Float
                     , color :: String }

data Drums = Drums { brand :: String
                   , price :: Float
                   , drumCount :: Int }


-- Question 2
-- Try to implement a data type that in its definition is refering to itself and make an
-- instace of it. And example of this is the implementation of the list data type.

