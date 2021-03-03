{-

Instructions for Running the Code
---------------------------------

For Linux:
1. Make sure GHC (Glasgow Haskell Compiler for Haskell) is installed on your machine.
2. Open the terminal and navigate to the directory containing the source code.
3. For compilation, type 'ghc quickSort.hs' and press enter. 
4. To run the file, type './quickSort' and press enter.
5. You will be prompted to enter an input.
6. Provide a valid input (a list of integers, e.g [12,2,4,5,18]) and then press enter.
7. The output (sorted list) will be displayed on the screen.

For Windows:
1. Make sure GHC (Glasgow Haskell Compiler for Haskell) is installed on your machine.
2. Open the command prompt and navigate to the directory containing the source code.
3. For compilation, type 'ghc .\quickSort.hs' and press enter. 
4. To run the file, type '.\quickSort.exe' and press enter.
5. You will be prompted to enter an input.
6. Provide a valid input (a list of integers, e.g [12,2,4,5,18]) and then press enter.
7. The output (sorted list) will be displayed on the screen.

NOTE
----

Appropriate error handling has been done. On entering an invalid input, an error message will be displayed to the user.

-}

-- importing the necessary packages to run the program
-- Text.Read for readMaybe
-- Data.Maybe for isJust and Maybe
import Text.Read
import Data.Maybe

-- function for checking if the input is a valid list of Int
checkValidList :: String -> Bool
checkValidList strList = isJust (readMaybe strList :: Maybe [Int])

-- function which sorts the given list using Quicksort
-- the last element of the list is taken as pivot
-- the remaining list is broken into two parts
-- first contains elements less than or equal to pivot
-- second contains elements greater than pivot
-- Quicksort is called recursively on the two parts
-- then the left part is merged with pivot element and then the right part
quickSort :: [Int] -> [Int]
-- if length of list is less than or equal to 1, return it (as it is already sorted)
quickSort a = if (length a <= 1)
              then a
-- else break the remaining list (all elements except last) into 2 parts and apply Quicksort recursively
              else (quickSort (filter (<= pivot) (remainingList))) ++ [pivot] ++ (quickSort (filter (> pivot) (remainingList)))
              where pivot = last a
                    remainingList = init a

main :: IO ()
main = do
    putStrLn "Enter list of numbers: "
--  taking a list of Ints as input in the form of a String from the user
    input <- getLine
--  checking if the input string can be parsed to a list of Ints 
    if(checkValidList input)
    then do
--      parsing the input string to a list of Ints
        let list = read input :: [Int]
--      applying Quicksort on this list and then printing the sorted list
        print(quickSort list)
--  if input is invalid, printing an error message
    else do
        print("Not a valid input")
        print("Input should be a list of integers")