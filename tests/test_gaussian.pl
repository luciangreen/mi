% test_gaussian.pl - Tests for Gaussian elimination and formula derivation
%:- use_module('../mi.pl').
:-include('../mi.pl').
:- begin_tests(gaussian_elimination).

test(simple_2x2_system) :-
    % System: x + y = 3, 2x + y = 5
    % Solution: x = 2, y = 1
    EquationSystem = [
        equation([1, 1], 3),
        equation([2, 1], 5)
    ],
    gaussian_eliminate_system(EquationSystem, Solutions),
    Solutions = [X, Y],
    abs(X - 2) < 0.001,
    abs(Y - 1) < 0.001,
    !.

test(polynomial_sum_1_to_n) :-
    % Derive formula for sum(1..n) = n*(n+1)/2
    % Data points: sum(0)=0, sum(1)=1, sum(2)=3, sum(3)=6
    % Polynomial form: a0 + a1*n + a2*n^2 + a3*n^3
    EquationSystem = [
        equation([1, 0, 0, 0], 0),  % n=0: a0 = 0
        equation([1, 1, 1, 1], 1),  % n=1: a0 + a1 + a2 + a3 = 1
        equation([1, 2, 4, 8], 3),  % n=2: a0 + 2*a1 + 4*a2 + 8*a3 = 3
        equation([1, 3, 9, 27], 6)  % n=3: a0 + 3*a1 + 9*a2 + 27*a3 = 6
    ],
    gaussian_eliminate_system(EquationSystem, Solutions),
    Solutions = [A0, A1, A2, A3],
    % Expected: 0 + 0.5*n + 0.5*n^2 + 0*n^3 = 0.5*n^2 + 0.5*n = n*(n+1)/2
    abs(A0) < 0.001,        % a0 ≈ 0
    abs(A1 - 0.5) < 0.001,  % a1 ≈ 0.5  
    abs(A2 - 0.5) < 0.001,  % a2 ≈ 0.5
    abs(A3) < 0.001,        % a3 ≈ 0
    !.

test(polynomial_sum_of_squares) :-
    % Derive formula for sum(1^2 + 2^2 + ... + n^2) = n*(n+1)*(2n+1)/6
    % Data points: n=0:0, n=1:1, n=2:5, n=3:14, n=4:30
    EquationSystem = [
        equation([1, 0, 0, 0, 0], 0),    % n=0
        equation([1, 1, 1, 1, 1], 1),    % n=1
        equation([1, 2, 4, 8, 16], 5),   % n=2
        equation([1, 3, 9, 27, 81], 14), % n=3
        equation([1, 4, 16, 64, 256], 30) % n=4
    ],
    gaussian_eliminate_system(EquationSystem, Solutions),
    length(Solutions, 5),
    Solutions = [A0, A1, A2, A3, _A4],
    % Expected: n*(n+1)*(2n+1)/6 = (2n^3 + 3n^2 + n)/6
    abs(A0) < 0.001,                    % a0 ≈ 0
    abs(A1 - 1/6) < 0.001,              % a1 ≈ 1/6
    abs(A2 - 0.5) < 0.001,              % a2 ≈ 0.5
    abs(A3 - 1/3) < 0.001,              % a3 ≈ 1/3
    !.

test(identity_matrix) :-
    % Simple identity system
    EquationSystem = [
        equation([1, 0, 0], 1),
        equation([0, 1, 0], 2),
        equation([0, 0, 1], 3)
    ],
    gaussian_eliminate_system(EquationSystem, Solutions),
    Solutions = [1, 2, 3],
    !.

:- end_tests(gaussian_elimination).

:- begin_tests(formula_extraction).

test(extract_linear_formula) :-
    % Extract formula from coefficients
    Coefficients = [0, 2, 0, 0],
    extract_closed_form(Coefficients, test_pred, Formula),
    % Should give 2*n
    compound(Formula),
    !.

test(extract_quadratic_formula) :-
    % Extract formula for n*(n+1)/2
    Coefficients = [0, 0.5, 0.5, 0],
    extract_closed_form(Coefficients, sum_pred, Formula),
    compound(Formula),
    !.

:- end_tests(formula_extraction).

% Run all tests
run_gaussian_tests :-
    run_tests([
        gaussian_elimination,
        formula_extraction
    ]).
