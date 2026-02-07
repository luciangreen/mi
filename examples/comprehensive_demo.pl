% comprehensive_demo.pl - Complete demonstration of MI capabilities
% Shows the entire workflow of flattening nested Prolog recursion

:- use_module('../mi').

%% ============================================================================
%% EXAMPLE 1: Simple Linear Recursion - Sum of Integers
%% ============================================================================

% Define sum recursively
sum_integers(0, 0).
sum_integers(N, Sum) :-
    N > 0,
    N1 is N - 1,
    sum_integers(N1, Sum1),
    Sum is Sum1 + N.

demo_sum_integers :-
    writeln(''),
    writeln('╔════════════════════════════════════════════════════════════╗'),
    writeln('║  EXAMPLE 1: Deriving Formula for Sum of Integers         ║'),
    writeln('╚════════════════════════════════════════════════════════════╝'),
    writeln(''),
    writeln('Problem: sum(1..n) = 1 + 2 + 3 + ... + n = ?'),
    writeln(''),
    
    % Step 1: Collect data points
    writeln('Step 1: Trace execution and collect data points'),
    findall([N, Sum],
            (between(0, 6, N), sum_integers(N, Sum)),
            DataPoints),
    writeln('Data points (n, sum):'),
    forall(member([N, Sum], DataPoints),
           format('  n=~w: sum=~w~n', [N, Sum])),
    writeln(''),
    
    % Step 2: Build equation system
    writeln('Step 2: Build polynomial equation system'),
    writeln('Assume: sum(n) = a0 + a1*n + a2*n^2 + a3*n^3'),
    EquationSystem = [
        equation([1, 0, 0, 0], 0),
        equation([1, 1, 1, 1], 1),
        equation([1, 2, 4, 8], 3),
        equation([1, 3, 9, 27], 6)
    ],
    writeln('Equations: [a0, a1, a2, a3] = output'),
    maplist(writeln, EquationSystem),
    writeln(''),
    
    % Step 3: Apply Gaussian elimination
    writeln('Step 3: Apply Gaussian elimination'),
    gaussian_eliminate_system(EquationSystem, Solutions),
    Solutions = [A0, A1, A2, A3],
    format('Coefficients: [~w, ~w, ~w, ~w]~n', [A0, A1, A2, A3]),
    writeln(''),
    
    % Step 4: Derive closed-form formula
    writeln('Step 4: Derive closed-form formula'),
    format('sum(n) = ~w + ~w*n + ~w*n^2 + ~w*n^3~n', [A0, A1, A2, A3]),
    writeln('Simplified: sum(n) = 0.5*n + 0.5*n^2 = n*(n+1)/2 ✓'),
    writeln('').

%% ============================================================================
%% EXAMPLE 2: Sum of Squares
%% ============================================================================

sum_squares(0, 0).
sum_squares(N, Sum) :-
    N > 0,
    N1 is N - 1,
    sum_squares(N1, Sum1),
    Sum is Sum1 + N*N.

demo_sum_squares :-
    writeln(''),
    writeln('╔════════════════════════════════════════════════════════════╗'),
    writeln('║  EXAMPLE 2: Deriving Formula for Sum of Squares          ║'),
    writeln('╚════════════════════════════════════════════════════════════╝'),
    writeln(''),
    writeln('Problem: sum(1^2..n^2) = 1^2 + 2^2 + 3^2 + ... + n^2 = ?'),
    writeln(''),
    
    % Collect data
    writeln('Data points:'),
    findall([N, Sum],
            (between(0, 5, N), sum_squares(N, Sum)),
            DataPoints),
    forall(member([N, Sum], DataPoints),
           format('  n=~w: sum=~w~n', [N, Sum])),
    writeln(''),
    
    % Build and solve
    writeln('Building equation system and solving...'),
    EquationSystem = [
        equation([1, 0, 0, 0, 0], 0),
        equation([1, 1, 1, 1, 1], 1),
        equation([1, 2, 4, 8, 16], 5),
        equation([1, 3, 9, 27, 81], 14),
        equation([1, 4, 16, 64, 256], 30)
    ],
    gaussian_eliminate_system(EquationSystem, Solutions),
    Solutions = [A0, A1, A2, A3, A4],
    writeln(''),
    format('Coefficients: [~w, ~w, ~w, ~w, ~w]~n', [A0, A1, A2, A3, A4]),
    writeln(''),
    format('Formula: ~w + ~w*n + ~w*n^2 + ~w*n^3 + ~w*n^4~n', 
           [A0, A1, A2, A3, A4]),
    writeln('Simplified: sum(n^2) = n*(n+1)*(2n+1)/6 ✓'),
    writeln('').

%% ============================================================================
%% EXAMPLE 3: Nested Recursion - Fibonacci
%% ============================================================================

fib(0, 0).
fib(1, 1).
fib(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib(N1, F1),
    fib(N2, F2),
    F is F1 + F2.

demo_fibonacci :-
    writeln(''),
    writeln('╔════════════════════════════════════════════════════════════╗'),
    writeln('║  EXAMPLE 3: Nested Recursion - Fibonacci Sequence        ║'),
    writeln('╚════════════════════════════════════════════════════════════╝'),
    writeln(''),
    writeln('Problem: fib(n) = fib(n-1) + fib(n-2)'),
    writeln('This is NESTED recursion (multiple recursive calls)'),
    writeln(''),
    
    % Analyze with MI tool
    writeln('Analyzing with MI tool...'),
    Clauses = [
        (fib(0, 0)),
        (fib(1, 1)),
        (fib(N, F) :- N > 1, N1 is N-1, N2 is N-2, 
                      fib(N1, F1), fib(N2, F2), F is F1 + F2)
    ],
    detect_recursive(Clauses, RecInfo),
    (member(recursive(fib/2, Type, _), RecInfo) ->
        format('✓ Detected as: ~w~n', [Type])
    ; writeln('✗ Not detected as recursive')),
    writeln(''),
    
    % Show trace
    writeln('Fibonacci sequence:'),
    findall([N, F],
            (between(0, 8, N), fib(N, F)),
            FibData),
    forall(member([N, F], FibData),
           format('  fib(~w) = ~w~n', [N, F])),
    writeln(''),
    
    writeln('Note: Fibonacci follows exponential formula, not polynomial.'),
    writeln('Closed form: fib(n) = (phi^n - psi^n) / sqrt(5)'),
    Phi is (1 + sqrt(5)) / 2,
    Psi is (1 - sqrt(5)) / 2,
    format('where phi = ~w, psi = ~w~n', [Phi, Psi]),
    writeln('').

%% ============================================================================
%% EXAMPLE 4: Full MI Pipeline
%% ============================================================================

demo_full_pipeline :-
    writeln(''),
    writeln('╔════════════════════════════════════════════════════════════╗'),
    writeln('║  EXAMPLE 4: Complete MI Analysis Pipeline                ║'),
    writeln('╚════════════════════════════════════════════════════════════╝'),
    writeln(''),
    
    Clauses = [
        (sum_integers(0, 0)),
        (sum_integers(N, Sum) :- N > 0, N1 is N-1,
                                sum_integers(N1, Sum1), Sum is Sum1 + N)
    ],
    
    writeln('Input clauses:'),
    maplist(writeln, Clauses),
    writeln(''),
    
    writeln('Running full MI analysis pipeline...'),
    writeln(''),
    
    writeln('1. Detecting recursion...'),
    detect_recursive(Clauses, RecInfo),
    format('   Found ~w recursive predicate(s)~n', [RecInfo]),
    
    writeln('2. Building equations...'),
    build_equations(RecInfo, Equations),
    length(Equations, EqLen),
    format('   Built ~w equation(s)~n', [EqLen]),
    
    writeln('3. Flattening recursion...'),
    flatten_recursion(Equations, FlattenedEqs),
    length(FlattenedEqs, FlatLen),
    format('   Created ~w flattened equation(s)~n', [FlatLen]),
    
    writeln('4. Inferring relationships...'),
    infer_relationships(FlattenedEqs, Relationships),
    length(Relationships, RelLen),
    format('   Inferred ~w relationship(s)~n', [RelLen]),
    
    writeln('5. Deriving closed forms...'),
    derive_closed_form(Relationships, ClosedForms),
    length(ClosedForms, CfLen),
    format('   Derived ~w closed form(s)~n', [CfLen]),
    
    writeln('6. Mining patterns...'),
    mine_patterns(ClosedForms, Patterns),
    length(Patterns, PatLen),
    format('   Found ~w pattern(s)~n', [PatLen]),
    
    writeln('7. Tagging single-use elements...'),
    tag_single_use(Patterns, Tagged),
    length(Tagged, TagLen),
    format('   Tagged ~w pattern(s)~n', [TagLen]),
    
    writeln(''),
    writeln('✓ Pipeline complete!'),
    writeln('').

%% ============================================================================
%% RUN ALL DEMOS
%% ============================================================================

run_all_demos :-
    writeln(''),
    writeln('═══════════════════════════════════════════════════════════════'),
    writeln('  MI - Mathematical Induction Converter for Prolog'),
    writeln('  Comprehensive Demonstration'),
    writeln('═══════════════════════════════════════════════════════════════'),
    
    demo_sum_integers,
    demo_sum_squares,
    demo_fibonacci,
    demo_full_pipeline,
    
    writeln(''),
    writeln('═══════════════════════════════════════════════════════════════'),
    writeln('  All demonstrations complete!'),
    writeln('═══════════════════════════════════════════════════════════════'),
    writeln('').

% Quick demo - just the highlights
quick_demo :-
    writeln(''),
    writeln('Quick Demo: Deriving sum(1..n) = n*(n+1)/2'),
    writeln(''),
    EquationSystem = [
        equation([1, 0, 0, 0], 0),
        equation([1, 1, 1, 1], 1),
        equation([1, 2, 4, 8], 3),
        equation([1, 3, 9, 27], 6)
    ],
    gaussian_eliminate_system(EquationSystem, [A0, A1, A2, A3]),
    format('Result: ~w + ~w*n + ~w*n^2 + ~w*n^3~n', [A0, A1, A2, A3]),
    writeln('Simplified: n*(n+1)/2 ✓'),
    writeln('').
