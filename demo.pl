% demo.pl - Demonstration script for MI converter
% Shows the mathematical induction converter in action

:- use_module(mi).

% Demo 1: Simple analysis of clauses
demo_basic :-
    writeln('=== Demo 1: Basic Clause Analysis ==='),
    Clauses = [
        (factorial(0, 1)),
        (factorial(N, R) :- N > 0, factorial(N-1, R1), R is N * R1)
    ],
    writeln('Input clauses:'),
    maplist(writeln, Clauses),
    nl,
    writeln('Detecting recursion...'),
    detect_recursive(Clauses, RecInfo),
    writeln('Recursive predicates found:'),
    maplist(writeln, RecInfo),
    nl.

% Demo 2: Full analysis pipeline
demo_pipeline :-
    writeln('=== Demo 2: Full Analysis Pipeline ==='),
    Clauses = [
        (fib(0, 0)),
        (fib(1, 1)),
        (fib(N, R) :- N > 1, fib(N-1, F1), fib(N-2, F2), R is F1 + F2)
    ],
    writeln('Analyzing Fibonacci...'),
    analyze_predicates(Clauses, Result),
    writeln('Analysis complete.'),
    length(Result, Len),
    format('Generated ~w tagged patterns~n', [Len]),
    nl.

% Demo 3: Pattern mining
demo_patterns :-
    writeln('=== Demo 3: Pattern Mining ==='),
    ClosedForms = [
        closed_form(pred1, formula(x * 2)),
        closed_form(pred2, formula(x * 2)),
        closed_form(pred3, formula(x * 2)),
        closed_form(pred4, formula(y + 1)),
        closed_form(pred5, formula(y + 1))
    ],
    writeln('Closed-form formulas:'),
    maplist(writeln, ClosedForms),
    nl,
    writeln('Mining patterns...'),
    mine_patterns(ClosedForms, Patterns),
    writeln('Patterns found:'),
    maplist(writeln, Patterns),
    nl,
    writeln('Tagging single-use elements...'),
    tag_single_use(Patterns, Tagged),
    writeln('Tagged patterns:'),
    maplist(writeln, Tagged),
    nl.

% Demo 4: File processing (if example file exists)
demo_file_processing :-
    writeln('=== Demo 4: File Processing ==='),
    InputFile = 'examples/factorial.pl',
    OutputFile = 'examples/factorial_output.pl',
    (exists_file(InputFile) ->
        (writeln('Processing file...'),
         process_file(InputFile, OutputFile),
         writeln('Processing complete!'),
         format('Output written to: ~w~n', [OutputFile]),
         format('Proof notes written to: ~w.proof~n', [OutputFile]))
    ; writeln('Example file not found. Skipping file processing demo.')
    ),
    nl.

% Run all demos
run_all_demos :-
    writeln(''),
    writeln('╔════════════════════════════════════════════════════════════╗'),
    writeln('║  MI - Mathematical Induction Converter for Prolog         ║'),
    writeln('║  Demonstration Script                                      ║'),
    writeln('╚════════════════════════════════════════════════════════════╝'),
    writeln(''),
    demo_basic,
    demo_pipeline,
    demo_patterns,
    demo_file_processing,
    writeln('All demonstrations complete!'),
    writeln('').

% Quick test - just show it works
quick_test :-
    writeln('Running quick test...'),
    Clauses = [(fact(0,1)), (fact(N,R) :- fact(N-1,R1), R is N*R1)],
    analyze_predicates(Clauses, Result),
    length(Result, Len),
    format('✓ Analysis complete. Generated ~w results.~n', [Len]).
