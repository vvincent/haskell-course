
import System.Directory (listDirectory, doesFileExist)
import System.CPUTime (getCPUTime)
import Data.List

-- Question 1
-- Define an IO action that count the number of files in the current directory 
-- and prints it to the terminal.

listFiles :: IO ()

-- In cryptography prime numbers, positive integers only divisible by itself and 1, play a fundamental role as 
-- they provide unbreakable mathematical structures. These structures are leverage to establish secure
-- cryptographic primitives like encryption!
--
-- But, generating primes is a computational straining problem, as we will measure in the following exercise.
-- This is because, to know whether a number is a prime number, you first need to know all the previous
-- primes, and then check that they are not a divisor of this number. So, this problem gets 
-- bigger and bigger! The goal of this exercise is to showcase different functional algorithms and their performance.
--
-- Below are three functions (primes1, primes2 and primes3) which all generate a list of prime numbers.

primes1 :: Integer -> [Integer]
primes1 m = sieve [2..m]
  where
    sieve [] = []
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

primes2 :: Integer -> [Integer]
primes2 m = sieve [2..m]
             where 
             sieve (x:xs) = x : sieve (xs \\ [x,x+x..m])
             sieve [] = []

primes3 :: Integer -> [Integer]
primes3 m = turner [2..m] 
    where
    turner []     = []
    turner (p:xs) = p : turner [x | x<-xs, x<p*p || rem x p /= 0]

-- Question 2
-- Define an IO action that calculates the time another IO action takes.
-- Use the getCPUTime :: IO Integer function to get the CPU time before and after the IO action.
-- The CPU time here is given in picoseconds (which 1/1000000000000th of a second).

timeIO :: IO a -> IO ()

-- Question 3
-- Write an `IO ()` action that retrieves an integer from the user via getLine.
-- Use read to parse this input and with it time the execution of retrieving the 
-- last element of the primes in `[2..m]`. 

benchmark :: IO ()

-- Question 4
-- Write an IO action that asks the user to type something and the program then 
-- writes the message to a file called msg.txt. After that it reads the text from
-- the msg.txt file and prints it back. Use the writeFile and readFile functions.

createMsg :: IO ()

{- Question 5 

Advanced example for students aiming at Plutus.
Write a program that prints the directory tree structure from the current
folder (denoted by "."). Below you can see an example output how such a structure looks like.  
.
├── foo1.hs
├── foo2.hs
├── bar1
    ├── bar2
    ├── foo3.hs
    ├── foo4.hs
    └── bar3
        └── foo5.hs
└── bar5
    ├── bar6
    ├── foo6.hs
    └── foo7.hs
HINT: From the System.Directory module you can use the function doesFileExist
which takes in a file path (String) and returns True if the item is a file. 
-}
