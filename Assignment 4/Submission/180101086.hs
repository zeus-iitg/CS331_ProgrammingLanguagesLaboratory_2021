{-

Instructions for Running the Code
---------------------------------

For Linux:
1. Make sure GHC (Glasgow Haskell Compiler for Haskell) is installed on your machine.
2. Open the terminal and navigate to the directory containing the source code.
3. For compilation, type 'ghc 180101086.hs' and press enter. 
4. To run the file, type './180101086' and press enter.
5. You will be prompted to enter an input.
6. Provide a valid input (a comma-separated list of Integers (with or without square brackets),
   e.g [12,2,4,5,18] or 12,2,4,5,18) and then press enter.
7. The various outputs (parsed list, LCM and BST traversals) will be displayed on the screen.

For Windows:
1. Make sure GHC (Glasgow Haskell Compiler for Haskell) is installed on your machine.
2. Open the command prompt and navigate to the directory containing the source code.
3. For compilation, type 'ghc .\180101086.hs' and press enter. 
4. To run the file, type '.\180101086.exe' and press enter.
5. You will be prompted to enter an input.
6. Provide a valid input (a comma-separated list of Integers (with or without square brackets),
   e.g [12,2,4,5,18] or 12,2,4,5,18) and then press enter.
7. The various outputs (parsed list, LCM and BST traversals) will be displayed on the screen.

NOTE
----

Appropriate error handling has been done. On entering an invalid input, an error message will be displayed to the user.

-}

-- importing the necessary packages to run the program
-- Text.Read for readMaybe
-- Data.Maybe for isJust and Maybe
import Text.Read
import Data.Maybe

-- function for checking if the input is a valid list of Integers
checkValidList :: String -> Bool
checkValidList strList = isJust (readMaybe strList :: Maybe [Integer])

-- function for computing GCD using recursive version of Euclidean algorithm
computeGCD :: Integer -> Integer -> Integer
computeGCD a b = if (b == 0)
                 then a
                 else computeGCD b (a `mod` b)

-- function for computing LCM of two non-negative integers based on the following result
-- a * b = GCD(a, b) * LCM(a, b)
-- this function uses computeGCD as a subroutine
-- corner cases have been handled where both numbers are 0
-- in that case, LCM will be 0
computeLCM :: Integer -> Integer -> Integer
computeLCM 0 0 = 0
computeLCM a b = (a * b) `div` (computeGCD a b)

-- function for computing LCM of a non-empty list of Integers
-- this function uses computeLCM as subroutine
-- since LCM(a, b) = LCM(|a|, |b|), we have passed abs(head a) to computeLCM
-- this will ensure correct answer even if (head a) is a negative integer
lcmList :: [Integer] -> Integer -> Integer
lcmList [] x = x
lcmList a x = lcmList (tail a) (computeLCM x (abs(head a)))

-- declaring a new type named BinarySearchTree using data keyword
-- it has two constructors
-- the first takes no argument and simply initializes the variable to Null
-- the second takes 3 arguments, which are described below
-- 1. an integer (representing the value stored in that node)
-- 2. a variable of type BinarySearchTree (representing the left child of current node)
-- 3. another variable of type BinarySearchTree (representing the right child of current node)
data BinarySearchTree = Null | Node Integer (BinarySearchTree) (BinarySearchTree)

-- function for inserting a node in a Binary Search Tree
-- the root is given as the first argument
-- the value to be stored in that node is given as the second argument
-- the function recursively searches for the correct position for the new node
-- if value inside current node is greater than the value of new node, we move to the left subtree of the current node
-- else we move to the right subtree of the current node
-- when we reach a Null value, we insert the node at that position
insert :: BinarySearchTree -> Integer -> BinarySearchTree
insert Null val = Node val Null Null
insert (Node x left right) val
    | x > val = Node x (insert left val) right
    | otherwise = Node x left (insert right val)

-- function for creating a Binary Search Tree from a list of Integers
makeBSTfromList :: [Integer] -> BinarySearchTree
makeBSTfromList xs = foldl insert Null xs

-- function which returns an Integer list containing preorder traversal of the BST
-- this is based on the recursive definition of preorder traversal of a BST
preorder :: BinarySearchTree -> [Integer]
preorder Null = []
preorder (Node x left right) = [x] ++ (preorder left) ++ (preorder right)

-- function which returns an Integer list containing inorder traversal of the BST
-- this is based on the recursive definition of inorder traversal of a BST
inorder :: BinarySearchTree -> [Integer]
inorder Null = []
inorder (Node x left right) = (inorder left) ++ [x] ++ (inorder right)

-- function which returns an Integer list containing postorder traversal of the BST
-- this is based on the recursive definition of postorder traversal of a BST
postorder :: BinarySearchTree -> [Integer]
postorder Null = []
postorder (Node x left right) = (postorder left) ++ (postorder right) ++ [x]

-- custom printer function built for printing an Integer list in desired format
-- here, I have chosen comma-separated values (csv) format
printer :: [Integer] -> IO()
printer [] = return ()
printer [x] = putStr (show x)
printer x = do
            putStr ((show (head x)) ++ ", ")
            printer (tail x)

main ::IO()
main = do

--  prompting the user to enter input (a comma-separated list of numbers (with or without square brackets))
    putStrLn "Enter a comma-separated list of numbers (with or without square brackets) : "

--  taking a list of Integers as input in the form of a String from the user
    input <- getLine

--  if input is a valid Integer list, it remains unchanged.
--  else we add square brackets at the start and end of the input
--  this enables us to support both types of input (with and without square brackets)
    let str = if (checkValidList input)
              then input 
              else "[" ++ input ++ "]"

--  checking if the string can be parsed to a list of Integers
    if(checkValidList str)
    then do

--      parsing the string to a list of Integers
        let list = read str :: [Integer]

--      printing the parsed list of Integers
        putStrLn ""
        putStrLn "Parsed list of Integers"
        putStr ("[")
        printer (list)
        putStr ("]")
        putStrLn "\n"

--      checking if list is non-empty
        if (length list) > 0
        then do

--          printing the LCM of the numbers inside the list
            let _lcm = lcmList list 1
            putStr "LCM : "
            print (_lcm)
            putStrLn ""

--          making a BST from the values inside the list
            let root = makeBSTfromList list

--          printing the various BST traversals
            putStrLn "Preorder traversal"
            printer (preorder root)
            putStrLn "\n"

            putStrLn "Inorder traversal"
            printer (inorder root)
            putStrLn "\n"

            putStrLn "Postorder traversal"
            printer (postorder root)
            putStrLn "\n"

        else do

--          if list is empty, printing error message indicating that
--          LCM cannot be computed and BST cannot be constructed
            putStrLn "Cannot compute LCM of empty list"
            putStrLn "Cannot construct Binary Search Tree from empty list"

--  if input is invalid, printing an error message
    else do
        putStrLn "Not a valid input"
        putStrLn "Input should be a comma-separated list of Integers (with or without square brackets)"