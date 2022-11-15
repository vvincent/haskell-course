
import System.Directory ( listDirectory )
import Data.Time.Clock ( diffUTCTime, getCurrentTime )

-- Question 1
-- Define an IO action that count the amount of files in the current directory 
-- and prints it to the terminal.


-- Question 2
-- Define an IO action that calculates the time an other IO action takes. 
-- HINT: use getCurrentTime and diffUTCTime from the module Data.Time.Clock


-- Question 3
-- Write an IO action that asks the user to type something and the program then 
-- writes the message to a file called msg.txt. After that it reads the text from
-- the msg.txt file and prints it back. Use the writeFile and readFile functions.

