{-# LANGUAGE ScopedTypeVariables #-}

module Homework15B where

import Control.Exception (IOException, try)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- IMPORTANT: Read the README.md file before completing the homework.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- 1. Write a function that takes a list and returns the head if the list is not empty.
-- If the list is empty, return Nothing.

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

-- 2. Write a function that takes a list of Maybe values and returns a list of all the Just values.
-- If there are no Just values, return an empty list.

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : xs) = catMaybes xs
catMaybes (Just x : xs) = x : catMaybes xs

-- 3. Write a function that tries to read from a file and returns the contents of the file.
-- If the file does not exist, return Nothing.

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe path = do
  contents <- try $ readFile path
  case contents of
    Left (_ :: IOException) -> return Nothing
    Right c -> return (Just c)

-- 4. Write a function that checks all the requirements for a password using the
-- Either type with a custom data type for errors.
-- The requirements are:
-- - The password must be at least 10 characters long.
-- - The password must contain at least one digit.
-- - The password must contain at least one uppercase letter.
-- - The password must contain at least one lowercase letter.

data PasswordError
  = NotLongEnough
  | NoDigit
  | NoUppercase
  | NoLowercase
  deriving (Eq, Show)

passwordLongEnough :: String -> Either PasswordError String
passwordLongEnough password =
  if length password >= 10
    then Right password
    else Left NotLongEnough

passwordHasDigit :: String -> Either PasswordError String
passwordHasDigit password =
  if any (`elem` ['0' .. '9']) password
    then Right password
    else Left NoDigit

passwordHasUppercase :: String -> Either PasswordError String
passwordHasUppercase password =
  if any (`elem` ['A' .. 'Z']) password
    then Right password
    else Left NoUppercase

passwordHasLowercase :: String -> Either PasswordError String
passwordHasLowercase password =
  if any (`elem` ['a' .. 'z']) password
    then Right password
    else Left NoLowercase

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- As you can see, the passwordRequirements function is very repetitive.
-- This is one of the downsides of using nested optional values.
-- We're going to solve this problem in the "Basic Abstractions" section of the coruse.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

passwordRequirements :: String -> Either PasswordError String
passwordRequirements password =
  case passwordLongEnough password of
    Left err -> Left err
    Right _ -> case passwordHasDigit password of
      Left err -> Left err
      Right _ -> case passwordHasUppercase password of
        Left err -> Left err
        Right _ -> case passwordHasLowercase password of
          Left err -> Left err
          Right _ -> Right password
