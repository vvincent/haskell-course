
{-
Question 1
Write a program for creating a shopping list, where the user can add 3 kinds of
flowers. When the program is started it should display the following message:
    Possible flowers are: daisy, sunflower and tulip.
    Possible actions are:
        add  --flower --amount 
        remove --flower --amout 
        show_list 
If the user does not use a valid comamnd and appropriate options notify him about
this. When the user says show_list print a message of how many of which flowers are
added to the list. Use a state monad when coding the solution.
-}

import Data.Map as Map ( fromList, insert, lookup, Map )
import Control.Monad.State ( evalState, runState, MonadState(put, get), State )
import Data.Maybe ( fromJust )
import Text.Read (readMaybe)

main :: IO ()
main = do
    putStrLn "Possible flowers are: daisy, sunflower and tulip."
    putStrLn "Possible actions are:\n \
            \ add  --flower --amount \n \
            \ remove --flower --amout \n \
            \ show_list \n"

    let emptyBasket = Map.fromList [("daisy", 0),("sunflower",0),("tulip",0)]
    shop emptyBasket

shop :: Map.Map String Int -> IO ()
shop basket = do
    putStrLn "What would you like to do:"
    fullCommand <- getLine

    if Prelude.null fullCommand
    then do
        putStrLn "No command was given."
        shop basket
    else do
        let (command:options) = words fullCommand
        case command of
            "add" -> do
                let (msg, (newBasket,_,_)) = runState updateOrder (basket, command, options)
                putStrLn msg
                shop newBasket
            "remove" -> do
                let (msg, (newBasket,_,_)) = runState updateOrder (basket, command, options)
                putStrLn msg
                shop newBasket
            "show_list" -> do
                print $ evalState updateOrder (basket, command, options)
                shop basket
            _ -> do
                putStrLn "Not a valid command."
                shop basket

updateOrder :: State (Map.Map String Int, String, [String]) String
updateOrder = do
    (basket, command, options) <- get
    if command == "show_list"
    then return $ describeBasket basket
    else do
        if length options /= 2
        then return "Options for this command should equal to 2."
        else do
            let flower = options !! 0
                amount = readMaybe (options !! 1) :: Maybe Int
            if flower `notElem` ["daisy","sunflower","tulip"] || amount == Nothing
            then return "The options for this command are not correct."
            else do
                let changeAmount = fromJust amount
                    currentAmount = fromJust $ Map.lookup flower basket
                case command of
                    "add" -> do
                        let updatedbasket = Map.insert flower (changeAmount + currentAmount) basket
                        put (updatedbasket, command, options)
                        return "Updated basket."
                    "remove" -> do
                        let updatedAmount = currentAmount - changeAmount
                        if updatedAmount < 0
                        then do
                            let updatedbasket = Map.insert flower 0 basket
                            put (updatedbasket, command, options)
                            return "Updated basket."
                        else do
                            let updatedbasket = Map.insert flower updatedAmount basket
                            put (updatedbasket, command, options)
                            return "Updated basket."
                    _ -> return "This case will anyway never happen"

describeBasket :: Map.Map String Int -> String
describeBasket basket = "The shopping list contains " ++ 
                        show daisy ++ " daisies, " ++
                        show sunflower ++ " sunflowers and " ++
                        show tulip ++ " tulips."
  where daisy = fromJust $ Map.lookup "daisy" basket
        sunflower = fromJust $ Map.lookup "sunflower" basket
        tulip = fromJust $ Map.lookup "tulip" basket
        