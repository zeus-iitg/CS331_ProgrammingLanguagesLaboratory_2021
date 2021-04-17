/*
  SHORTEST PATH IN A MAZE
  
  Example:
  shortest_path(0, 99).
*/

% declaring all the dynamic predicates (can be added and removed at runtime)
:- dynamic(visited/1).
:- dynamic(distance/2).
:- dynamic(faultynode/1).
:- dynamic(parent/2).

% checks if source and destination are not faulty nodes
ok(Src, Dst) :- \+faultynode(Src),
                \+faultynode(Dst),
                !.

% this handles the case when X is the source node
% in that case, we do not have to use any recursion
print_path(X) :- parent(X, -1),
                 format('~d ', [X]),
                 !.
% this handles the case when X is not the source node
% in that case, we recursively print path from source
% to parent of X and then we print X
print_path(X) :- \+parent(X, -1),
                 parent(X, Y),
                 print_path(Y),
                 format('~d ', [X]),
                 !.
% if destination is reachable, it prints the
% shortest distance from source to destination
print_result(X) :- distance(X, Y),
                   format('Distance = ~d\n',[Y]),
                   format('Path = '),
                   print_path(X),
                   format('\n'),
                   !.
% if destination is unreachable, it prints a
% 'Destination unreachable' message on the screen
print_result(X) :- \+distance(X, _),
                   format('Destination unreachable\n'),
                   !.

% before starting bfs, we remove any leftover predicates
% from previous bfs runs and then do the following
% 1. Mark the source node as visited
% 2. Set the distance of source node to 0
% 3. Set the parent of source node to -1
% 4. Insert source node in bfs queue
% 5. Continue the bfs with current queue
start_bfs(Src) :- retractall(visited(_)),
                  retractall(distance(_ , _)),
                  retractall(parent(_, _)),
                  assertz(visited(Src)),
                  assertz(distance(Src, 0)),
                  assertz(parent(Src, -1)),
                  continue_bfs([Src]),
                  !.

% in case a node has no valid neighbours, do nothing
process_adjacent([], _) :- !.
% in case a node has valid neighbours, for every neighbour do
% 1. Mark neighbour as visited
% 2. Set the parent of neighbour to node
% 3. Set distance of neighbour to (distance(node) + 1)
% 4. Process the remaining neighbours
process_adjacent([X | Y], Parent) :- distance(Parent, D),
                                     assertz(visited(X)),
                                     assertz(parent(X, Parent)),
                                     E is D+1,
                                     assertz(distance(X, E)),
                                     process_adjacent(Y, Parent),
                                     !.

% if bfs queue is empty, do nothing
continue_bfs([]) :- !.
% if bfs queue is non-empty, let X be the front node of the queue
% find all valid neighbours of X (non faulty, non visited)
% process all the valid neighbours of X
% add the list of neighbours of X to the remaining queue (queue without X)
% continue bfs with the new queue
continue_bfs([X | Y]) :- findall(Next_node, (mazelink(X, Next_node), \+ faultynode(Next_node), \+ visited(Next_node)), Adjacent),
                         process_adjacent(Adjacent, X),
                         append(Y, Adjacent, Z),
                         continue_bfs(Z),
                         !.

% if either source node or destination node is faulty, display an error message
shortest_path(Src, Dst) :- \+ok(Src, Dst),
                           format('Source and Destination cannot be faulty\n'),
                           !.

% if both source nodes and destination nodes are non faulty
% start a bfs from source node
% after bfs ends, print the result
shortest_path(Src, Dst) :- ok(Src, Dst),
                           start_bfs(Src),
                           print_result(Dst),
                           !.