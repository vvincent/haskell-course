
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.ByteString as BS ( ByteString, pack, unpack, putStrLn, length )
import Data.ByteString.Char8 as BC ( unpack )

{-
Question 1
Write a function that takes in some text and a word and returns the number of words
and lines the input text contains and the number of occurences of the input word. 
-}

textStatistics :: T.Text -> T.Text -> IO ()
textStatistics text word = do
    Prelude.putStrLn $ "The text contains " ++ show wordCount ++ " words."
    Prelude.putStrLn $ "The text contains " ++ show lineCount ++ " lines."
    TIO.putStrLn $ T.intercalate  "" ["The word ", word, " appears ", T.pack $ show occurances, " times."]
  where wordCount = Prelude.length $ T.words text
        lineCount = Prelude.length $ T.lines text
        occurances = Prelude.length (T.splitOn word text) - 1

sampleText :: T.Text
sampleText = "This is some text.\n The text goes accross multiple lines."

test1 :: IO ()
test1 = textStatistics sampleText "text"

{-
Question 2
Create a function that converts the binary representation of a ByteString into an Integer using a base 256 conversion.

Example: we can convert [4,255,50] in base 256 to base 10 as 

                                50*(256)^0 + 255*(256)^1 + 4*(256)^2. 

Hint: since Haskell byte strings are a list of Word8 (base 256 integers) check out: toInteger :: Word8 -> Integer. 
-}

byteStringToInteger :: BS.ByteString -> Integer
byteStringToInteger b = let wrds = BS.unpack b
                        in  go wrds  
                        where 
                            go [] = 0
                            go xs = toInteger(xs !! (Prelude.length xs - 1)) + 256 * go (Prelude.init xs)

test2 :: IO ()
test2 = do
    let hello = "Hello World!" :: BS.ByteString
    print $ byteStringToInteger hello

{-
Question 3
Create a function that converts Integers into the binary representation using a base 256 conversion.
For simplicity we will not represent the parity of the integers (the negative integers). 
You can use the abs function to make all inputs positive.

Example: we can convert 3000000 in base 10 to base 256 as 

        [((300000 `div` 256) `div` 256) `mod` 256,(300000 `div` 256) `mod` 256, 300000 `mod` 256]. 

Hint: since Haskell byte strings are a list of Word8 (base 256 integers) check out: toEnum :: Int -> Word8. 
-}

integerToByteString :: Integer -> BS.ByteString
integerToByteString n = BS.pack $ go (abs n)
                        where
                            go 0 = []
                            go m = go (m `div` 256) ++ [(toEnum . fromIntegral) (m `mod` 256)] 

test3 :: IO ()
test3 = do
    let hello = "Hello World!" :: BS.ByteString
        integerHello = byteStringToInteger hello
    print $ BC.unpack $ integerToByteString integerHello
