-- This homework is around creating Haskell types that represent wines from over the world.

-- Question 1
-- Different wines are made from different grapes, there are around 10000 varieties over the world!
-- Create a type synonym called "Grape" for the different grape names as strings.
-- Additionally, use this type synonym for the grapes: "Sangiovese", "Cabernet-sauvignon", "Merlot" and "Garnacha".
type Grape = String
grapeList' = ["Sangiovese", "Cabernet-sauvignon", "Merlot", "Garnacha"] :: [Grape]


-- Question 2
-- The most famous regions that export wine are located in France, Italy and Spain.
-- Each of these countries is divided up in smaller regions.
-- These smaller regions are known for a certain style, for example the Champagne region in France
-- Create a type synonym called "Region" for wine region given their country and region as a tuple of strings.
-- Additionally, use this type synonym for the regions: Bordeaux in France, Tuscany in Italy and Rioja in Spain.
type Region = (String, String)

regions = [("Bordeaux","France"), ("Tuscany","Italy"), ("Rioja","Spain")] :: [Region]


-- Question 3
-- A wine is either one of three kinds, these are red, white or rose wine.
-- Besides its kind, each wine also has a given alcohol level.
-- Create a data type called "Kind" that represents these three kinds, with each capturing the level of alcohol.
-- Additionally, use this data type for the examples: red wine with 14.5% alcohol, white wine with 13% alcohol 
-- and Rose wine with 12% alcohol.
type AlcoholLevel = Float
data Kind = Red AlcoholLevel | White AlcoholLevel | Rose AlcoholLevel deriving (Show)

redWine = Red 14.5 :: Kind
wines = [Red 14.5, White 13.0, Rose 12.0] :: [Kind]

-- Question 4
-- In the world of wines, bottles display all of the above information for the consumer on its label.
-- Create a record type called "Label" that captures the grapes that are in a whine, the region its from,
-- and it's kind. Notice that some wines are a blended combination of multiple grapes!
-- Additionally, create for each of the described wine below a label.
data Label = Label { grapes :: [Grape], region :: Region, kind :: Kind } deriving (Show)

-- Larrosa Rose is a rose wine from the region Rioja. It is made from the Garnacha grape and 
-- has a alcohol level of 14%.
larrosaRose = Label ["Garnacha"] ("Rioja","Spain") (Rose 14.0)

-- Castiglioni is a red wine from the region of Tuscany. It is made from the grape Sangiovese and
-- has an alcohol level of 12.5%.
castiglioni = Label { grapes = ["Sangiovese"], region = ("Tuscany","Italy"), kind = Red 12.5 }

-- Bordeaux is known for its red wine, these are mainly a blend between Cabernet-sauvignon and Merlot.
-- Create a Label for the wine "Le Petit Haut Lafitte" that has an alcohol percentage 13.5%.
lePetitHautLafitte = Label ["Cabernet-sauvignon", "Merlot"] ("Bordeaux","France") (Red 13.5)

-- Question 5
-- Write a function `containsGrape` that takes a list of Labels and a Grape and returns a boolean.
-- The function should check if the there exists a wine in the Label that contains this Grape.
containsGrape :: [Label] -> Grape -> Bool
containsGrape [] _ = False
containsGrape (Label {grapes=gs}:xs) g
  | contains gs g = True
  | otherwise = containsGrape xs g
  where
    contains [] _ = False
    contains (z:zs) r
      | z == r = True
      | otherwise = contains zs r


-- This is a test list for the `containsGrape` function with an grape that is not in the list.
grapeList = [larrosaRose,castiglioni,lePetitHautLafitte] :: [Label]
newGrape = "Merlot"

-- the best solution from copilot
containsGrape' :: [Label] -> Grape -> Bool
containsGrape' labels g = any (elem g . grapes) labels

-- without function composition and point-free style. the best one 
containsGrape'' :: [Label] -> Grape -> Bool
containsGrape'' labels g = any (\label -> elem g (grapes label)) labels

-- another solution using a lambda function with pattern matching
containsGrape''' :: [Label] -> Grape -> Bool
containsGrape''' labels grape = any (\Label{grapes=xs} -> grape `elem` xs) labels