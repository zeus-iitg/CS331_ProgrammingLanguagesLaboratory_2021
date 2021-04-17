Instructions for Execution
--------------------------

For Linux:
1. Make sure SWI Prolog is installed on your machine.
2. Open the terminal and navigate to the directory containing the .pl file.
3. Type "swipl" and hit enter to open SWI Prolog interactive interpreter. 
4. To load the database, type "consult('180101086.pl')." and press enter.
5. To load Mazedata file, type "consult('Mazedata.pl')."
6. To compute shortest path from source to destination in a maze, use
                          shortest_path(Source, Destination).
   For example:           shortest_path(0, 99).

For Windows:
1. Make sure SWI Prolog application is installed on your machine.
2. Open SWI-Prolog application.
3. Click on File -> Consult.
4. Navigate to the directory containing the .pl file.
5. Open "180101086.pl".
6. Repeat steps 3 to 5 to load "Mazedata.pl".
7. To compute shortest path from source to destination in a maze, use
                          shortest_path(Source, Destination).
   For example:           shortest_path(0, 99).

NOTE
----

Appropriate error handling has been done. If either the source node or destination node is faulty, 
an error message will be displayed to the user. If destination is unreachable, a message stating 
'Destination unreachable' is displayed to the user.