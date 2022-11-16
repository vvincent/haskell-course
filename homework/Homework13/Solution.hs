
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString as BS ( ByteString, pack, unpack )
import Data.ByteString.Char8 as BC ( unpack )

{-
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

main1 :: IO ()
main1 = do
    let hello = "Hello World!" :: BS.ByteString
    print $ byteStringToInteger hello

{-
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

main2 :: IO ()
main2 = do
    let hello = "Hello World!" :: BS.ByteString
        integerHello = byteStringToInteger hello
    print $ BC.unpack $ integerToByteString integerHello
