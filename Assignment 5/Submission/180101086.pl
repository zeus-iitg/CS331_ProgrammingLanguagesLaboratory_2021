/*
  SQUAREROOT
  
  Example:
  squareroot(23, 0.0001).
*/

% checks whether NUM is less than or equal to
% the square root of N or not
ok(NUM, N) :- (NUM * NUM) =< N, !.

% if L is a valid answer
% print it
binSrch(L, _, N, EPS) :- N - (L * L) < EPS, format('~15f', [L]), !.

% if L is not a valid answer
% if MID is less than or equal to the square root of N, then continue the search on the interval [MID, R]
binSrch(L, R, N, EPS) :- N - (L * L) >= EPS, MID is (L + R) / 2, ok(MID, N), binSrch(MID, R, N, EPS), !.
% if MID is greater than the square root of N, continue the search on the interval [L, MID]
binSrch(L, R, N, EPS) :- N - (L * L) >= EPS, MID is (L + R) / 2, \+(ok(MID, N)), binSrch(L, MID, N, EPS), !.

% if input is invalid
% if N is negative, print an error message
squareroot(N, _) :- N < 0, print('Square root of a negative number doesn\'t exist'), !.
% if EPS is negative or zero, print an error message
squareroot(_, EPS) :- EPS =< 0, print('Kindly enter a positive value of accuracy'), !.

% if input is valid
% if N >= 1, binary search for the square root on the interval [0, N]
squareroot(N, EPS) :- N >= 1, EPS > 0, binSrch(0, N, N, EPS), !.
% if 0 <= N < 1, binary search for the square root on the interval [0, 1]
squareroot(N, EPS) :- N < 1, N >= 0, EPS > 0, binSrch(0, 1, N, EPS), !.

/*
  IS_SUBLIST
  
  Example:
  is_sublist([1, 2, 3], [4, 8, 1, 7, 2, 9, 3, 0]).
*/

% base cases
% empty list is a sublist of every list
is_sublist([], _) :- write('Yes'), !.
% non-empty list cannot be a sublist of an empty list
is_sublist(X, []) :- X \= [], write('No'), !.

% if first element of both lists are same
% first list is a sublist of the second list if
% tail of first list is a sublist of tail of second list
is_sublist([X | Y], [X | Z]) :- is_sublist(Y, Z), !.

% if first element of both lists are different
% first list is a sublist of the second list if
% first list is a sublist of tail of second list
is_sublist([P | Q], [R | S]) :- P \= R, is_sublist([P | Q], S), !.