% Example: Factorial function (recursive)
% This demonstrates nested recursion that can be converted to induction form

factorial(0, 1).
factorial(N, Result) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, R1),
    Result is N * R1.

% Example: Fibonacci (nested recursive)
fibonacci(0, 0).
fibonacci(1, 1).
fibonacci(N, Result) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fibonacci(N1, F1),
    fibonacci(N2, F2),
    Result is F1 + F2.

% Example: List length (tail recursive)
list_length([], 0).
list_length([_|T], N) :-
    list_length(T, N1),
    N is N1 + 1.

% Example: Sum of list (recursive with accumulator)
sum_list([], Sum, Sum).
sum_list([H|T], Acc, Sum) :-
    NewAcc is Acc + H,
    sum_list(T, NewAcc, Sum).
