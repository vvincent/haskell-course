{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module User.Actions.Battle where
import System.Random (randomRIO)

data Golem = Golem { gAttack :: Int, gHp :: Int } deriving Show
data Player = Player { pAttack :: Int, pHp :: Int } deriving (Show)
data Battle = Fight | RunAway deriving (Show, Read)

battle :: IO Bool
battle = do
  putStrLn "\nYou've encountered a Golem! Choose an action: Fight or RunAway?"
  gatt <- randomRIO @Int (1, 5)
  ghp  <- randomRIO @Int (10, 50)
  patt <- randomRIO @Int (3, 7)
  php  <- randomRIO @Int (30, 70)
  let enemy = Golem { gAttack = gatt, gHp = ghp }
  let player = Player { pAttack = patt, pHp = php }
  battleLoop enemy player
  where
    battleLoop (Golem {gHp}) _ | gHp <= 0 = putStrLn "You've won the battle!" >> return True
    battleLoop _ (Player {pHp}) | pHp <= 0 = putStrLn "You've lost the battle and died -.-!" >> return False
    battleLoop (Golem{..}) (Player{..}) = do
      putStrLn $ "\nThe Golem has " ++ show gHp ++ " health and " ++ show gAttack ++ " attack."
      putStrLn $ "You have " ++ show pHp ++ " health and " ++ show pAttack ++ " attack."
      putStrLn "Choose an action: Fight, RunAway."
      selection <- getLine
      case read selection of
        Fight -> do
          gLuck <- randomRIO (1, 3)
          pLuck <- randomRIO (1, 3)
          let gHit = gAttack * gLuck
              pHit = pAttack * pLuck
              newGolem = Golem { gAttack = gAttack, gHp = gHp - pHit }
              newPlayer = Player { pAttack = pAttack, pHp = pHp - gHit }
          battleLoop newGolem newPlayer
        RunAway -> do
          luck <- randomRIO @Int (1, 3)
          case luck of
            2 -> putStrLn "You've managed to run away!" >> return True
            _ -> do
              putStrLn "You've failed to run away! And the Golem hit you!"
              gLuck <- randomRIO (1, 3)
              let newPlayer = Player { pAttack = pAttack, pHp = pHp - (gAttack * gLuck) }
              battleLoop (Golem{..}) newPlayer