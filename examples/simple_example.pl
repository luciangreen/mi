% simple_example.pl - A simpler example to demonstrate pattern detection
% This file contains predicates with obvious patterns

% Simple doubling function
double(X, Y) :- Y is X * 2.

% Another doubling (pattern should be detected)
double_list([], []).
double_list([H|T], [H2|T2]) :-
    H2 is H * 2,
    double_list(T, T2).

% Triple function
triple(X, Y) :- Y is X * 3.
