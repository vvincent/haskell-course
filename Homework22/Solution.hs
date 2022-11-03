
-- Question 1
-- TODO

import Data.Map as Map
import Control.Monad.State
import Data.Maybe
import Text.Read (readMaybe)

main :: IO ()
main = do
    putStrLn "Possible flowers are: daisy, sunflower and tulip."
    putStrLn "Possible actions are:\n \
            \ add  --flower --amount \n \
            \ remove --flower --amout \n \
            \ list_order \n"

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
            "list_order" -> do
                print $ evalState updateOrder (basket, command, options)
                shop basket
            _ -> do
                putStrLn "Not a valid command."
                shop basket

updateOrder :: State (Map.Map String Int, String, [String]) String
updateOrder = do
    (basket, command, options) <- get
    if command == "list_order"
    then return $ describeBasket basket
    else do
        if length options /= 2
        then return "Options for this command should equal to 2."
        else do
            let flower = options !! 0
                amount = readMaybe (options !! 0) :: Maybe Int
            if flower `notElem` ["daisy","sunflower","tulip"] || amount == Nothing
            then return "The options for this command are not correct."
            else do
                case command of
                    "add" -> do
                        let updatedbasket = Map.insert flower (fromJust amount + 1) basket
                        put (updatedbasket, command, options)
                        return "Updated basket."
                    "remove" -> do
                        let changeAmount = fromJust amount
                            currentAmount = fromJust $ Map.lookup flower basket
                        if currentAmount - changeAmount < 0
                        then do
                            let updatedbasket = Map.insert flower 0 basket
                            put (updatedbasket, command, options)
                            return "Updated basket."
                        else do
                            let updatedbasket = Map.insert flower (fromJust amount - 1) basket
                            put (updatedbasket, command, options)
                            return "Updated basket."
                    _ -> return "This case will anyway never happen"

describeBasket :: Map.Map String Int -> String
describeBasket basket = "The basket contains " ++ 
                        show daisy ++ " daisies, " ++
                        show sunflower ++ " sunflowers and " ++
                        show tulip ++ " tulips."
  where daisy = fromJust $ Map.lookup "daisy" basket
        sunflower = fromJust $ Map.lookup "sunflower" basket
        tulip = fromJust $ Map.lookup "tulip" basket