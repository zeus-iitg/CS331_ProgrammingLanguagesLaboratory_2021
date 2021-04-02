Instructions for Execution
--------------------------

For Linux:
1. Make sure gprolog is installed on your machine.
2. Open the terminal and navigate to the directory containing the .pl file.
3. Type "gprolog" and hit enter to open GNU Prolog interactive interpreter. 
4. To load the database, type "consult('180101086.pl')." and press enter.
5. To compute square root of a number, use
                         squareroot(NUMBER, ACCURACY).
   For example:          squareroot(23, 0.0001).
6. To check if a list is a sublist of another list, use
                         is_sublist(LIST_1, LIST_2).
                         is_sublist([1, 2, 3], [4, 8, 1, 7, 2, 9, 3, 0]).

For Windows:
1. Make sure GNU Prolog application is installed on your machine.
2. Navigate to the directory containing the .pl file in Windows Explorer.
3. Open 180101086.pl with GNU Prolog application. 
4. To compute square root of a number, use
                         squareroot(NUMBER, ACCURACY).
   For example:          squareroot(23, 0.0001).
5. To check if a list is a sublist of another list, use
                         is_sublist(LIST_1, LIST_2).
                         is_sublist([1, 2, 3], [4, 8, 1, 7, 2, 9, 3, 0]).

NOTE
----

Appropriate error handling has been done. On entering an invalid input, an error message will be displayed to the user.
