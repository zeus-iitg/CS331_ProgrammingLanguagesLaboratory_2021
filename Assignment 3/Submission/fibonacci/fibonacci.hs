{-

Instructions for Running the Code
---------------------------------

For Linux:
1. Make sure GHC (Glasgow Haskell Compiler for Haskell) is installed on your machine.
2. Open the terminal and navigate to the directory containing the source code.
3. For compilation, type 'ghc fibonacci.hs' and press enter. 
4. To run the file, type './fibonacci' and press enter.
5. You will be prompted to enter an input.
6. Provide a valid input (a non-negative integer, e.g 200) and then press enter.
7. The output (nth fibonacci number) will be displayed on the screen.

For Windows:
1. Make sure GHC (Glasgow Haskell Compiler for Haskell) is installed on your machine.
2. Open the command prompt and navigate to the directory containing the source code.
3. For compilation, type 'ghc .\fibonacci.hs' and press enter. 
4. To run the file, type '.\fibonacci.exe' and press enter.
5. You will be prompted to enter an input.
6. Provide a valid input (a non-negative integer, e.g 200) and then press enter.
7. The output (nth fibonacci number) will be displayed on the screen.

NOTE
----

Appropriate error handling has been done. On entering an invalid input, an error message will be displayed to the user.

-}

-- importing the necessary packages to run the program
-- Text.Read for readMaybe
-- Data.Maybe for isJust and Maybe
import Text.Read
import Data.Maybe

-- function for checking if the input is a valid Int
checkValidInt :: String -> Bool
checkValidInt strInt = isJust (readMaybe strInt :: Maybe Int)

-- function for computing nth fibonacci number
-- it takes n as input and returns a pair of Integers
-- for n>0, the pair is (fib (n-1), fib n)
fib :: Int -> (Integer, Integer)
fib 0 = (0, 0)
fib 1 = (0, 1)
-- (fib (n-1), fib (n-2) + fib (n-1))
fib n = (snd x, fst x + snd x)
        where x = fib (n-1)

main :: IO ()
main = do
    putStrLn "Enter value of n: "
--  taking an Int as input in the form of a String from the user
    input <- getLine
--  checking if the input string can be parsed to an Int 
    if(checkValidInt input)
    then do
--      parsing the input string to an Int
        let n = (read input :: Int)
--      if the entered value of n is valid (non-negative)
        if(n >= 0)
--      printing the value of the nth fibonacci number
        then print(snd (fib n))
--      else printing an error message to user
        else do
            print("Not a valid input")
            print("n should be a non-negative integer")
--  in case the input cannot be parsed to an Int, print an error message to the user
    else do
        print("Not a valid input")
        print("n should be a non-negative integer")