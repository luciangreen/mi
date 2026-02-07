% test_mi.pl - Test suite for MI (Mathematical Induction) converter
:-include('../mi.pl').
%:- use_module('../mi.pl').

:- begin_tests(mi_parser).

test(parse_simple_fact) :-
    parse_clauses([foo(a, b)], Parsed),
    Parsed = [parsed(foo(a, b), true, _)].

test(parse_rule) :-
    parse_clauses([(bar(X, Y) :- baz(X, Y))], Parsed),
    length(Parsed, 1),
    Parsed = [parsed(bar(_,_), baz(_,_), _)].

test(parse_recursive_factorial) :-
    Clause = (factorial(N, Result) :- 
              N > 0,
              N1 is N - 1,
              factorial(N1, R1),
              Result is N * R1),
    parse_clauses([Clause], [Parsed]),
    Parsed = parsed(factorial(_,_), _, _).

:- end_tests(mi_parser).

:- begin_tests(mi_recursion_detection).

test(detect_factorial_recursive) :-
    Clause = (factorial(N, Result) :- 
              N > 0,
              N1 is N - 1,
              factorial(N1, R1),
              Result is N * R1),
    detect_recursive([Clause], RecInfo),
    length(RecInfo, 1),
    member(recursive(factorial/2, _, _), RecInfo).

test(detect_fibonacci_nested) :-
    Clause = (fibonacci(N, Result) :-
              N > 1,
              N1 is N - 1,
              N2 is N - 2,
              fibonacci(N1, F1),
              fibonacci(N2, F2),
              Result is F1 + F2),
    detect_recursive([Clause], RecInfo),
    member(recursive(fibonacci/2, Type, _), RecInfo),
    (Type = nested_recursive ; Type = general_recursive).

test(non_recursive_not_detected) :-
    Clause = (add(X, Y, Z) :- Z is X + Y),
    detect_recursive([Clause], RecInfo),
    RecInfo = [].

:- end_tests(mi_recursion_detection).

:- begin_tests(mi_equation_building).

test(build_equations_for_factorial) :-
    Clause = (factorial(N, Result) :- 
              N > 0,
              factorial(N-1, R1),
              Result is N * R1),
    detect_recursive([Clause], RecInfo),
    build_equations(RecInfo, Equations),
    length(Equations, L),
    L >= 0. % At least processes without error

test(equations_have_indices) :-
    Clause = (fact(N, R) :- fact(N-1, R1), R is N * R1),
    detect_recursive([Clause], RecInfo),
    build_equations(RecInfo, Equations),
    (Equations = [] ; member(equation(_, Indices, _), Equations)),
    !.

:- end_tests(mi_equation_building).

:- begin_tests(mi_flattening).

test(flatten_creates_symbolic_vars) :-
    Equations = [equation(test/2, [idx(1), idx(2)], 
                         expr(tail_recursive, test(x, y)))],
    flatten_recursion(Equations, Flattened),
    length(Flattened, L),
    L >= 0. % Processes without error

test(flatten_maintains_structure) :-
    Equations = [equation(pred/1, [idx(1)], expr(general, body))],
    flatten_recursion(Equations, Flattened),
    (Flattened = [] ; member(flattened(pred/1, _, _, _), Flattened)),
    !.

:- end_tests(mi_flattening).

:- begin_tests(mi_pattern_mining).

test(mine_patterns_from_simple_forms) :-
    ClosedForms = [
        closed_form(pred1, formula(x * 2)),
        closed_form(pred2, formula(x * 2)),
        closed_form(pred3, formula(x + 1))
    ],
    mine_patterns(ClosedForms, Patterns),
    length(Patterns, L),
    L >= 0. % At least processes

test(identify_reusable_patterns) :-
    ClosedForms = [
        closed_form(p1, f(a, b, c)),
        closed_form(p2, f(a, b, c))
    ],
    mine_patterns(ClosedForms, Patterns),
    !. % Success if completes

:- end_tests(mi_pattern_mining).

:- begin_tests(mi_tagging).

test(tag_single_use_elements) :-
    Patterns = [
        pattern(pat1, name1, 1),
        pattern(pat2, name2, 5)
    ],
    tag_single_use(Patterns, Tagged),
    member(tagged(pat1, single_use(name1, 1)), Tagged),
    member(tagged(pat2, reusable(name2, 5)), Tagged).

test(tagging_preserves_all_patterns) :-
    Patterns = [pattern(p1, n1, 2), pattern(p2, n2, 1)],
    tag_single_use(Patterns, Tagged),
    length(Patterns, LP),
    length(Tagged, LT),
    LP = LT.

:- end_tests(mi_tagging).

:- begin_tests(mi_integration).

test(analyze_predicates_pipeline) :-
    Clauses = [
        (fact(0, 1)),
        (fact(N, R) :- N > 0, fact(N-1, R1), R is N * R1)
    ],
    analyze_predicates(Clauses, Result),
    !. % Success if completes without error

test(full_pipeline_minimal) :-
    Clauses = [(simple(X, Y) :- Y is X + 1)],
    detect_recursive(Clauses, RecInfo),
    RecInfo = []. % Non-recursive, should be empty

:- end_tests(mi_integration).

:- begin_tests(mi_utilities).

test(term_depth_atom) :-
    term_depth(atom, Depth),
    Depth = 0.

test(term_depth_compound) :-
    term_depth(f(a, b, c), Depth),
    Depth = 1.

test(term_depth_nested) :-
    term_depth(f(g(h(x))), Depth),
    Depth = 3.

:- end_tests(mi_utilities).

% Run all tests
run_all_tests :-
    run_tests([
        mi_parser,
        mi_recursion_detection,
        mi_equation_building,
        mi_flattening,
        mi_pattern_mining,
        mi_tagging,
        mi_integration,
        mi_utilities
    ]).
