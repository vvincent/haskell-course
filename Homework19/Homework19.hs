----------------------------------------------------------------------------------------------------
---------------------------------------- Question 1 ------------------------------------------------
{-
-- Write the specialized types of all the `Functor` and `Applicative` operators
-- in the `result` expression.
-}

eDiv :: Float -> Float -> Either String Float
eDiv x 0 = Left "division by zero"
eDiv x y = Right (x / y)



--   Either String (Float -> Float -> Float) -> Either String Float -> Either String (Float -> Float)
--                                               |
result :: Either String Float --                 |
result = (\x y z -> x * y + z) <$> (3 `eDiv` 2) <*> Right 2 <*> pure 4
--                              |                            |_______________________________________________
--                              |                                                                           |
-- (Float -> Float -> Float -> Float) -> Either String Float -> Either String (Float -> Float -> Float)     |
--                                                                                                          |
--                                                   _______________________________________________________|
--                                                  |
--                  Either String (Float -> Float) -> Either String Float -> Either String Float



----------------------------------------------------------------------------------------------------
---------------------------------------- Question 2 ------------------------------------------------
{-
TODO
-}
