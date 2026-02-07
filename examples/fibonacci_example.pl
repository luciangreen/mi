% Example: Fibonacci - Nested Recursion
% This demonstrates handling nested recursion by flattening multiple recursive calls
% Fibonacci is defined as: fib(n) = fib(n-1) + fib(n-2)
% Closed form: fib(n) = (phi^n - psi^n) / sqrt(5)
% where phi = (1 + sqrt(5))/2, psi = (1 - sqrt(5))/2

:- use_module('../mi').

% Recursive definition
fib(0, 0).
fib(1, 1).
fib(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib(N1, F1),
    fib(N2, F2),
    F is F1 + F2.

% Demo: Derive formula for Fibonacci using Gaussian elimination
demo_fibonacci_formula :-
    writeln('=== Deriving Formula for Fibonacci (Nested Recursion) ==='),
    writeln(''),
    writeln('Fibonacci sequence: fib(n) = fib(n-1) + fib(n-2)'),
    writeln(''),
    
    % Collect data points
    writeln('Computing Fibonacci numbers...'),
    fib(0, F0), format('fib(0) = ~w~n', [F0]),
    fib(1, F1), format('fib(1) = ~w~n', [F1]),
    fib(2, F2), format('fib(2) = ~w~n', [F2]),
    fib(3, F3), format('fib(3) = ~w~n', [F3]),
    fib(4, F4), format('fib(4) = ~w~n', [F4]),
    fib(5, F5), format('fib(5) = ~w~n', [F5]),
    fib(6, F6), format('fib(6) = ~w~n', [F6]),
    fib(7, F7), format('fib(7) = ~w~n', [F7]),
    writeln(''),
    
    % Build equation system
    % Note: Fibonacci follows exponential growth, not polynomial
    % We'll use polynomial approximation or demonstrate the method
    writeln('Building equation system (polynomial approximation)...'),
    EquationSystem = [
        equation([1, 0, 0, 0, 0], F0),
        equation([1, 1, 1, 1, 1], F1),
        equation([1, 2, 4, 8, 16], F2),
        equation([1, 3, 9, 27, 81], F3),
        equation([1, 4, 16, 64, 256], F4)
    ],
    
    writeln('Applying Gaussian elimination...'),
    gaussian_eliminate_system(EquationSystem, Solutions),
    writeln('Solutions (polynomial coefficients):'),
    writeln(Solutions),
    writeln(''),
    
    % Extract coefficients
    (Solutions = [A0, A1, A2, A3, A4|_] ->
        format('Polynomial approximation:~n', []),
        format('  fib(n) ≈ ~w + ~w*n + ~w*n^2 + ~w*n^3 + ~w*n^4~n', [A0, A1, A2, A3, A4]),
        writeln(''),
        writeln('Note: Fibonacci actually follows exponential formula:'),
        Phi is (1 + sqrt(5)) / 2,
        Psi is (1 - sqrt(5)) / 2,
        format('  fib(n) = (phi^n - psi^n) / sqrt(5)~n', []),
        format('  where phi = ~w, psi = ~w~n', [Phi, Psi])
    ;
        writeln('Could not extract coefficients')
    ),
    writeln('').

% Demo: Show how nested recursion is flattened step by step
demo_nested_flattening :-
    writeln('=== Flattening Nested Recursion Step-by-Step ==='),
    writeln(''),
    writeln('Original: fib(n) = fib(n-1) + fib(n-2)'),
    writeln(''),
    
    writeln('Step 1: Trace recursive calls'),
    writeln('  For n=5:'),
    writeln('    fib(5) calls fib(4) and fib(3)'),
    writeln('    fib(4) calls fib(3) and fib(2)'),
    writeln('    fib(3) calls fib(2) and fib(1)'),
    writeln('    ... and so on'),
    writeln(''),
    
    writeln('Step 2: Build equations from outputs'),
    writeln('  Let F(n) be the output for input n'),
    writeln('  F(0) = 0'),
    writeln('  F(1) = 1'),
    writeln('  F(2) = F(1) + F(0) = 1'),
    writeln('  F(3) = F(2) + F(1) = 2'),
    writeln('  F(4) = F(3) + F(2) = 3'),
    writeln('  F(5) = F(4) + F(3) = 5'),
    writeln(''),
    
    writeln('Step 3: Find pattern using Gaussian elimination'),
    writeln('  Try polynomial: F(n) = a0 + a1*n + a2*n^2 + ...'),
    writeln('  or exponential: F(n) = c1*r1^n + c2*r2^n'),
    writeln(''),
    
    writeln('Step 4: For nested recursion, we handle each recursive branch:'),
    writeln('  Branch 1: fib(n-1) -> leads to formula F1(n)'),
    writeln('  Branch 2: fib(n-2) -> leads to formula F2(n)'),
    writeln('  Combined: fib(n) = F1(n-1) + F2(n-2)'),
    writeln(''),
    
    writeln('Step 5: Merge branches into single formula'),
    writeln('  Substitute n-1 and n-2 into formulas'),
    writeln('  Combine and simplify'),
    writeln('  Result: Single closed-form formula'),
    writeln('').

% Demonstrate analyzing nested recursion with the MI tool
demo_analyze_fibonacci :-
    writeln('=== Analyzing Fibonacci with MI Tool ==='),
    writeln(''),
    
    Clauses = [
        (fib(0, 0)),
        (fib(1, 1)),
        (fib(N, F) :- N > 1, N1 is N-1, N2 is N-2, 
                     fib(N1, F1), fib(N2, F2), F is F1 + F2)
    ],
    
    writeln('Detecting recursion type...'),
    detect_recursive(Clauses, RecInfo),
    (member(recursive(fib/2, Type, _), RecInfo) ->
        format('Fibonacci is ~w~n', [Type])
    ;
        writeln('Recursion type not detected')
    ),
    writeln(''),
    
    writeln('Full analysis...'),
    analyze_predicates(Clauses, Result),
    length(Result, NumPatterns),
    format('Generated ~w pattern(s)~n', [NumPatterns]),
    writeln('').

% Run all demos
run_demos :-
    demo_nested_flattening,
    demo_fibonacci_formula,
    demo_analyze_fibonacci.
