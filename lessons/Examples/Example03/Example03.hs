{-
    -- if-then-else expressions (1)
    if <expression> then <expression> else <expression>    
-}
adultOrMinor :: Int -> String
adultOrMinor age = if age < 18 then "you are a minor" else "you are an adult"

{-
    -- if-then-else expressions (2)
    if <expression> 
        then <expression>    
        else <expression>
-}
coldOrWarm :: Int -> String
coldOrWarm temp =
    if temp < 60 
        then "cold"        
        else "warm"

coldOrWarmOrHot :: Int -> String
coldOrWarmOrHot temp =
    if temp < 60
        then "cold"
        else 
            if temp < 80
                then "warm"
                else "hot"    


{-
    -- Guards
    func arg
        | <expression> = <expression>
        | <expression> = <expression>
        | otherwise = <expression>
-}
coldOrWarmOrHot' :: Int -> String
coldOrWarmOrHot' temp
    | temp < 60 = "cold"
    | temp < 80 = "warm"
    | otherwise = "hot"

{-
    -- let expressions (1)
    let <variable> = <expression> in <expression that uses the variable>    
-}
volumeOfCylinder :: Double -> Double -> Double
volumeOfCylinder radius height = let areaOfCircle r = pi * r^2 in areaOfCircle radius * height

{-
    -- let expressions (2)
    let <variable1> = <expression> 
        <variable2> = <expression>
    in <expression that uses the variable1 and/or variable2>
-}
haitiDollarToUSD :: Double -> Double
haitiDollarToUSD haitiDollars =
    let haitiDollarToHaitiGourde hd = hd * 5
        haitiGourdeToUSD hg = hg / 131.07
    in haitiGourdeToUSD (haitiDollarToHaitiGourde haitiDollars)

{-
    -- where expressions
    func arg = <expression that uses the variables>
        where <variable1> = <expression>
              <variable2> = <expression>        
-}
haitiDollarToUSD' :: Double -> Double
haitiDollarToUSD' haitiDollars = haitiGourdeToUSD (haitiDollarToHaitiGourde haitiDollars)
    where haitiDollarToHaitiGourde hd = hd * 5
          haitiGourdeToUSD hg = hg / 131.07

{-
    -- where expressions with guards
    func arg 
            | <expression> = <expression>
            | <expression> = <expression>
            | otherwise = <expression>
        where <variable1> = <expression>
              <variable2> = <expression>
-}
haitiDollarToUSD'' :: Double -> String
haitiDollarToUSD'' haitiDollars  
        | usd > 10 = "You are rich!" ++ " You have " ++ show usd ++ " USD."
        | otherwise = "You are poor!" ++ " You have " ++ show usd ++ " USD."
    where haitiDollarToHaitiGourde hd = hd * 5
          haitiGourdeToUSD hg = hg / 131.07
          usd = haitiGourdeToUSD (haitiDollarToHaitiGourde haitiDollars)

{-
    -- anorther way to write where expressions
    func arg = <expression that uses the variable>
        where <variable> = <expression>
-}
fToK :: Double -> Double
fToK v = 273.15 + fC v
  where fC t = (t - 32) * 5 / 9

{-
    -- if-then-else expressions with let expressions
    if <expression> 
    then let <variable> = <expression> in <expression that uses the variable>
    else let <variable> = <expression> in <expression that uses the variable>
-}
initials :: String -> String -> String
initials name lastName = if name == "" || lastName == ""
                         then "How was your name again?"
                         else let x = head name
                                  y = head lastName
                              in [x] ++ "." ++ [y] ++ "."
        