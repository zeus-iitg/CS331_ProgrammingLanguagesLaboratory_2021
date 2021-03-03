{-

Instructions for Running the Code
---------------------------------

For Linux:
1. Make sure GHC (Glasgow Haskell Compiler for Haskell) is installed on your machine.
2. Open the terminal and navigate to the directory containing the source code.
3. For compilation, type 'ghc sqrt.hs' and press enter. 
4. To run the file, type './sqrt' and press enter.
5. You will be prompted to enter an input.
6. Provide a valid input (a non-negative real number, e.g 23.56) and then press enter.
7. The output (value of square root up to 5 decimal places) will be displayed on the screen.

For Windows:
1. Make sure GHC (Glasgow Haskell Compiler for Haskell) is installed on your machine.
2. Open the command prompt and navigate to the directory containing the source code.
3. For compilation, type 'ghc .\sqrt.hs' and press enter. 
4. To run the file, type '.\sqrt.exe' and press enter.
5. You will be prompted to enter an input.
6. Provide a valid input (a non-negative real number, e.g 23.56) and then press enter.
7. The output (value of square root up to 5 decimal places) will be displayed on the screen.

NOTE
----

Appropriate error handling has been done. On entering an invalid input, an error message will be displayed to the user.

-}

-- importing the necessary packages to run the program
-- Text.Read for readMaybe
-- Data.Maybe for isJust and Maybe
-- Text.Printf for printf
import Text.Read
import Data.Maybe
import Text.Printf

-- function for truncating a Double up to 5 decimal places
-- it ensures that the value is not rounded up while truncation
round5dp :: Double -> Double
round5dp num = (fromIntegral (floor (num * 100000))) / 100000

-- function for checking if the input is a valid Double
checkValidDouble :: String -> Bool
checkValidDouble strDouble = isJust (readMaybe strDouble :: Maybe Double)

-- function for checking if a is less than or equal to the square root of b or not
ok :: Double -> Double -> Bool
ok a b = ((a * a) <= b)

-- function which uses binary search to lower the search space for square root of n
-- unless the width of the search space becomes less than eps
--          left      right      n        eps       ans
binSrch :: Double -> Double -> Double -> Double -> Double
-- if width of the search space becomes less than eps, return r
binSrch l r n eps = if (r - l < eps)
                    then r
-- else check if (l + r) / 2 is less than or equal to the square root of n or not
                    else if(ok mid n)
-- if it is less than or equal to the square root of n, make mid as the left end of search space
                    then binSrch mid r n eps
-- else make mid as the right end of the search space
                    else binSrch l mid n eps
                    where mid = (l + r) / 2

main :: IO ()
main = do
    putStrLn "Enter value of n: "
--  taking a Double as input in the form of a String from the user
    input <- getLine
--  checking if the input string can be parsed to a Double 
    if(checkValidDouble input)
    then do
--      parsing the input string to a Double
        let n = (read input :: Double)
--      checking if the entered number is non-negative or not
        if(n >= 0)
--      computing square root if it is a non-negative number
        then do
            let eps = 0.000001
--          if number is greater than or equal to 1 then square root lies between 0 and n 
            if(n >= 1)
            then printf "%0.5f" (round5dp (binSrch 0 n n eps))
--          else square root lies between 0 and 1 (and is greater than or equal to n)
            else printf "%0.5f" (round5dp (binSrch 0 1 n eps))
--      if number is negative, print an error message stating that square root does not exist
        else print("Square root of a negative number doesn't exist")
--  if invalid input is entered, print an error message to the user
    else do
        print("Not a valid input")
        print("n should be a non-negative real number")