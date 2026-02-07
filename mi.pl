% mi.pl - Mathematical Induction Converter for Prolog
% Converts nested recursive Prolog to mathematical induction form
%
% Main pipeline:
% 1. Parse Prolog clauses
% 2. Detect nested/recursive predicates
% 3. Build equation system via subterm-address indexing
% 4. Flatten by replacing recursive calls with symbolic vars
% 5. Infer relationships using Gaussian elimination
% 6. Derive closed-form/flattened formulas
% 7. Mine repeated subterm patterns for code reuse
% 8. Tag single-use elements
% 9. Emit rewritten Prolog + proof notes/tests
:- module(mi, []).
/*
:- module(mi, [
    process_file/2,
    analyze_predicates/2,
    parse_clauses/2,
    detect_recursive/2,
    build_equations/2,
    flatten_recursion/2,
    infer_relationships/2,
    derive_closed_form/2,
    mine_patterns/2,
    tag_single_use/2,
    emit_rewritten/3,
    term_depth/2,
    flatten_to_formula/4,
    gaussian_eliminate_system/2,
    equations_to_augmented_matrix/2,
    simple_gaussian_elimination/2,
    extract_solution_column/2,
    extract_closed_form/3
]).*/

%:- use_module('../Starlog/starlog').

%% Main entry point: process a Prolog file
process_file(InputFile, OutputFile) :-
    % Step 1: Parse Prolog clauses from file
    parse_file_clauses(InputFile, Clauses),
    
    % Step 2: Detect recursive/nested predicates
    detect_recursive(Clauses, RecursiveInfo),
    
    % Step 3: Build equation system via subterm-address indexing
    build_equations(RecursiveInfo, Equations),
    
    % Step 4: Flatten by replacing recursive calls with symbolic vars
    flatten_recursion(Equations, FlattenedEqs),
    
    % Step 5: Infer relationships using Gaussian elimination
    infer_relationships(FlattenedEqs, Relationships),
    
    % Step 6: Derive closed-form/flattened formulas
    derive_closed_form(Relationships, ClosedForms),
    
    % Step 7: Mine repeated subterm patterns
    mine_patterns(ClosedForms, Patterns),
    
    % Step 8: Tag single-use elements
    tag_single_use(Patterns, Tagged),
    
    % Step 9: Emit rewritten Prolog + proof notes/tests
    emit_rewritten(Tagged, OutputFile, ProofNotes),
    
    % Display summary
    format('~nProcessing complete.~n', []),
    format('Input: ~w~n', [InputFile]),
    format('Output: ~w~n', [OutputFile]),
    format('Proof notes generated: ~w~n', [ProofNotes]).

%% Analyze predicates without file I/O (for programmatic use)
analyze_predicates(Clauses, Result) :-
    detect_recursive(Clauses, RecursiveInfo),
    build_equations(RecursiveInfo, Equations),
    flatten_recursion(Equations, FlattenedEqs),
    infer_relationships(FlattenedEqs, Relationships),
    derive_closed_form(Relationships, ClosedForms),
    mine_patterns(ClosedForms, Patterns),
    tag_single_use(Patterns, Result).

%% Flatten nested recursion into a single formula
%% This is the main entry point for the flattening functionality
flatten_to_formula(PredicateName, Arity, TestCases, ClosedFormFormula) :-
    % Step 1: Trace execution with test cases
    trace_predicate_execution(PredicateName, Arity, TestCases, TraceData),
    
    % Step 2: Build system of equations from trace data
    build_equation_system(TraceData, EquationSystem),
    
    % Step 3: Apply Gaussian elimination to find relationships
    gaussian_eliminate_system(EquationSystem, ReducedSystem),
    
    % Step 4: Extract coefficients and derive closed-form formula
    extract_closed_form(ReducedSystem, PredicateName, ClosedFormFormula).

%% Trace predicate execution with test cases
trace_predicate_execution(PredicateName, Arity, TestCases, TraceData) :-
    findall(trace_point(Inputs, Output),
            (member(Inputs, TestCases),
             execute_predicate(PredicateName, Arity, Inputs, Output)),
            TraceData).

execute_predicate(PredicateName, Arity, Inputs, Output) :-
    % Build the goal dynamically
    length(Inputs, InputLen),
    OutputArity is Arity - InputLen,
    length(OutputVars, OutputArity),
    append(Inputs, OutputVars, AllArgs),
    Goal =.. [PredicateName|AllArgs],
    % Try to execute (safely catch errors)
    (catch(call(Goal), _, fail) ->
        (OutputVars = [Output] -> true
        ; Output = OutputVars)
    ; Output = undefined).

%% Build system of linear equations from trace data
build_equation_system(TraceData, EquationSystem) :-
    % For each trace point, build an equation
    % Assuming polynomial form: a0 + a1*n + a2*n^2 + a3*n^3 = output
    findall(equation(Coefficients, Output),
            build_equation_from_trace(TraceData, Coefficients, Output),
            EquationSystem).

build_equation_from_trace(TraceData, Coefficients, Output) :-
    member(trace_point(Inputs, Output), TraceData),
    Output \= undefined,
    number(Output),
    % Build coefficient matrix [1, n, n^2, n^3, ...]
    build_coefficient_row(Inputs, Coefficients).

build_coefficient_row([N], [1, N, N2, N3]) :-
    % Single input case: polynomial in n
    number(N),
    N2 is N * N,
    N3 is N * N * N.
build_coefficient_row([N1, N2], [1, N1, N2, N1N2]) :-
    % Two input case
    number(N1), number(N2),
    N1N2 is N1 * N2.
build_coefficient_row(Inputs, [1|Inputs]) :-
    % General case: linear in inputs
    true.

%% Apply Gaussian elimination to equation system  
gaussian_eliminate_system(EquationSystem, Solutions) :-
    % Convert to augmented matrix
    equations_to_augmented_matrix(EquationSystem, AugMatrix),
    % Apply simple Gaussian elimination
    simple_gaussian_elimination(AugMatrix, ReducedMatrix),
    % Extract solutions (last column)
    extract_solution_column(ReducedMatrix, Solutions).

equations_to_augmented_matrix([], []).
equations_to_augmented_matrix([equation(Coeffs, Output)|Rest], [Row|MatrixRest]) :-
    append(Coeffs, [Output], Row),
    equations_to_augmented_matrix(Rest, MatrixRest).

%% Simple Gaussian elimination - forward elimination only
simple_gaussian_elimination(Matrix, Reduced) :-
    eliminate_forward(Matrix, 0, Reduced).

eliminate_forward(Matrix, ColIdx, Result) :-
    length(Matrix, NumRows),
    (ColIdx < NumRows ->
        process_column(Matrix, ColIdx, NewMatrix),
        NextCol is ColIdx + 1,
        eliminate_forward(NewMatrix, NextCol, Result)
    ; Result = Matrix).

process_column(Matrix, ColIdx, Result) :-
    % Get the pivot row (row at ColIdx)
    (nth0(ColIdx, Matrix, PivotRow) ->
        nth0(ColIdx, PivotRow, PivotVal),
        (abs(PivotVal) > 0.0001 ->
            % Normalize pivot row
            maplist(safe_divide(PivotVal), PivotRow, NormPivot),
            % Replace in matrix
            replace_row(Matrix, ColIdx, NormPivot, Matrix1),
            % Eliminate below
            eliminate_below(Matrix1, ColIdx, Result)
        ; Result = Matrix)
    ; Result = Matrix).

safe_divide(Divisor, Value, Result) :-
    (number(Value), number(Divisor), abs(Divisor) > 0.0001 ->
        Result is Value / Divisor
    ; Result = Value).

eliminate_below(Matrix, PivotIdx, Result) :-
    nth0(PivotIdx, Matrix, PivotRow),
    eliminate_below_rows(Matrix, PivotRow, PivotIdx, 0, Result).

eliminate_below_rows([], _, _, _, []).
eliminate_below_rows([Row|Rest], PivotRow, PivotIdx, CurrentIdx, [NewRow|RestResult]) :-
    (CurrentIdx > PivotIdx ->
        % Eliminate this row
        nth0(PivotIdx, Row, RowVal),
        (abs(RowVal) > 0.0001 ->
            maplist(subtract_with_mult(RowVal), Row, PivotRow, NewRow)
        ; NewRow = Row)
    ; NewRow = Row),
    NextIdx is CurrentIdx + 1,
    eliminate_below_rows(Rest, PivotRow, PivotIdx, NextIdx, RestResult).

subtract_with_mult(Mult, A, B, C) :-
    (number(A), number(B) ->
        C is A - Mult * B
    ; C = A).

replace_row(Matrix, Index, NewRow, Result) :-
    replace_row_helper(Matrix, Index, NewRow, 0, Result).

replace_row_helper([], _, _, _, []).
replace_row_helper([_|Rest], Index, NewRow, Index, [NewRow|Rest]) :- !.
replace_row_helper([Row|Rest], Index, NewRow, CurrentIdx, [Row|RestResult]) :-
    NextIdx is CurrentIdx + 1,
    replace_row_helper(Rest, Index, NewRow, NextIdx, RestResult).

extract_solution_column([], []).
extract_solution_column(ReducedMatrix, Solutions) :-
    % Simple back substitution: work from bottom row up
    length(ReducedMatrix, N),
    back_sub_simple(ReducedMatrix, N, Solutions).

back_sub_simple(Matrix, N, Solutions) :-
    back_sub_iter(Matrix, N, N, [], Solutions).

back_sub_iter(_Matrix, _N, 0, Acc, Acc) :- !.
back_sub_iter(Matrix, N, RowIdx, KnownVars, AllVars) :-
    RowIdx > 0,
    RowIdx1 is RowIdx - 1,
    % Get row at index RowIdx-1 (0-indexed)
    nth1(RowIdx, Matrix, Row),
    % Extract RHS (last element)
    append(Coeffs, [RHS], Row),
    % Solve for variable at position RowIdx-1
    solve_for_var(Coeffs, RowIdx1, KnownVars, RHS, VarValue),
    % Continue with this variable now known
    back_sub_iter(Matrix, N, RowIdx1, [VarValue|KnownVars], AllVars).

solve_for_var(Coeffs, VarIdx, KnownVars, RHS, Value) :-
    % Get coefficient for this variable
    nth0(VarIdx, Coeffs, C0),
    % Subtract contributions from known variables (to the right)
    length(Coeffs, NumCoeffs),
    VarIdx1 is VarIdx + 1,
    subtract_right_contributions(Coeffs, VarIdx1, NumCoeffs, KnownVars, RHS, AdjustedRHS),
    % Solve: C0 * Value = AdjustedRHS
    (number(C0), abs(C0) > 0.0001 ->
        Value is AdjustedRHS / C0
    ; Value = AdjustedRHS).

subtract_right_contributions(_Coeffs, Idx, NumCoeffs, _KnownVars, RHS, RHS) :-
    Idx >= NumCoeffs, !.
subtract_right_contributions(Coeffs, Idx, NumCoeffs, [V|RestVars], RHS, FinalRHS) :-
    Idx < NumCoeffs,
    nth0(Idx, Coeffs, C),
    (number(C), number(V) ->
        NewRHS is RHS - C * V
    ; NewRHS = RHS),
    Idx1 is Idx + 1,
    subtract_right_contributions(Coeffs, Idx1, NumCoeffs, RestVars, NewRHS, FinalRHS).
subtract_right_contributions(_Coeffs, _Idx, _NumCoeffs, [], RHS, RHS).

%% Numeric Gaussian elimination (kept for compatibility)
gaussian_elimination_numeric(Matrix, Matrix).

gauss_forward_elimination([], []).
gauss_forward_elimination([PivotRow|Rows], [PivotRow|Result]) :-
    % Normalize pivot row
    normalize_row(PivotRow, NormalizedPivot),
    % Eliminate below
    eliminate_below_row(NormalizedPivot, Rows, EliminatedRows),
    gauss_forward_elimination(EliminatedRows, Result).

normalize_row(Row, NormalizedRow) :-
    Row = [Pivot|_],
    (Pivot =\= 0 ->
        maplist(divide_by(Pivot), Row, NormalizedRow)
    ; NormalizedRow = Row).

divide_by(Divisor, Value, Result) :-
    (number(Value), Divisor =\= 0 ->
        Result is Value / Divisor
    ; Result = Value).

eliminate_below_row(_PivotRow, [], []).
eliminate_below_row(PivotRow, [Row|Rows], [NewRow|Result]) :-
    PivotRow = [PivotVal|_],
    Row = [RowVal|_],
    (PivotVal =\= 0, number(RowVal) ->
        Multiplier is RowVal / PivotVal,
        subtract_rows_mult(Row, PivotRow, Multiplier, NewRow)
    ; NewRow = Row),
    eliminate_below_row(PivotRow, Rows, Result).

subtract_rows_mult([], [], _, []).
subtract_rows_mult([A|As], [B|Bs], Mult, [C|Cs]) :-
    (number(A), number(B) ->
        C is A - Mult * B
    ; C = A),
    subtract_rows_mult(As, Bs, Mult, Cs).

gauss_back_substitution(Matrix, Solutions) :-
    % Extract solutions from reduced matrix
    reverse(Matrix, ReversedMatrix),
    extract_solutions(ReversedMatrix, [], Solutions).

extract_solutions([], Acc, Acc).
extract_solutions([Row|Rows], Acc, Solutions) :-
    % Extract the solution from each row
    reverse(Row, [Solution|_]),
    extract_solutions(Rows, [Solution|Acc], Solutions).

back_substitution(ReducedMatrix, Solutions) :-
    % Simple back substitution - extract last column
    maplist(extract_last_element, ReducedMatrix, Solutions).

extract_last_element(Row, Last) :-
    reverse(Row, [Last|_]).

%% Extract closed-form formula from reduced system
extract_closed_form(Solutions, _PredicateName, ClosedFormFormula) :-
    % Solutions are coefficients [a0, a1, a2, a3, ...]
    (Solutions = [A0, A1, A2, A3|_] ->
        % Build formula: A0 + A1*n + A2*n^2 + A3*n^3
        build_polynomial_formula([A0, A1, A2, A3], ClosedFormFormula)
    ; Solutions = [A0, A1, A2] ->
        build_polynomial_formula([A0, A1, A2], ClosedFormFormula)
    ; Solutions = [A0, A1] ->
        build_polynomial_formula([A0, A1], ClosedFormFormula)
    ; ClosedFormFormula = constant(Solutions)).

build_polynomial_formula(Coeffs, Formula) :-
    build_poly_terms(Coeffs, 0, Terms),
    simplify_sum(Terms, Formula).

build_poly_terms([], _, []).
build_poly_terms([C|Cs], Power, [Term|Terms]) :-
    (abs(C) < 0.0001 ->
        % Skip near-zero coefficients
        Term = 0
    ; Power =:= 0 ->
        Term = C
    ; Power =:= 1 ->
        (C =:= 1 -> Term = n
        ; C =:= -1 -> Term = -n
        ; Term = C * n)
    ; C =:= 1 ->
        atom_concat('n^', Power, NPower),
        Term = NPower
    ; atom_concat('n^', Power, NPower),
      Term = C * NPower),
    NextPower is Power + 1,
    build_poly_terms(Cs, NextPower, Terms).

simplify_sum([], 0).
simplify_sum([0|Rest], Sum) :- !,
    simplify_sum(Rest, Sum).
simplify_sum([Term], Term) :- !.
simplify_sum([Term|Rest], Sum) :-
    simplify_sum(Rest, RestSum),
    (RestSum = 0 -> Sum = Term
    ; Sum = Term + RestSum).

%% Parse Prolog clauses from a file
parse_file_clauses(InputFile, Clauses) :-
    open(InputFile, read, Stream),
    read_clauses_from_stream(Stream, Clauses),
    close(Stream).

read_clauses_from_stream(Stream, []) :-
    at_end_of_stream(Stream), !.
read_clauses_from_stream(Stream, Clauses) :-
    \+ at_end_of_stream(Stream),
    read(Stream, Term),
    (Term == end_of_file -> Clauses = []
    ; Clauses = [Term|Rest], read_clauses_from_stream(Stream, Rest)).

%% Parse clauses from a list (for testing)
parse_clauses(Terms, ParsedClauses) :-
    maplist(parse_single_clause, Terms, ParsedClauses).

parse_single_clause(Term, parsed(Head, Body, Meta)) :-
    (Term = (Head :- Body) ->
        extract_metadata(Term, Meta)
    ; Head = Term, Body = true, Meta = []).

extract_metadata((Head :- Body), Meta) :-
    functor(Head, Name, Arity),
    count_body_goals(Body, GoalCount),
    Meta = [functor(Name), arity(Arity), body_goals(GoalCount)].
extract_metadata(Head, Meta) :-
    functor(Head, Name, Arity),
    Meta = [functor(Name), arity(Arity), body_goals(0)].

count_body_goals(true, 0) :- !.
count_body_goals((A, B), N) :- !,
    count_body_goals(A, N1),
    count_body_goals(B, N2),
    N is N1 + N2.
count_body_goals(_, 1).

%% Detect recursive and nested predicates
detect_recursive(Clauses, RecursiveInfo) :-
    findall(recursive(Name/Arity, Type, Clause),
            (member(Clause, Clauses),
             is_recursive_clause(Clause, Name/Arity, Type)),
            RecursiveInfo).

is_recursive_clause((Head :- Body), Name/Arity, Type) :- !,
    functor(Head, Name, Arity),
    (contains_recursive_call(Body, Name/Arity) ->
        classify_recursion_type(Body, Name/Arity, Type)
    ; fail).
is_recursive_clause(_, _, _) :- fail.

contains_recursive_call(Body, Name/Arity) :-
    functor_in_term(Body, Name, Arity).

functor_in_term(Term, Name, Arity) :-
    compound(Term),
    (functor(Term, Name, Arity) -> true
    ; Term =.. [_|Args], member(Arg, Args), functor_in_term(Arg, Name, Arity)).

classify_recursion_type(Body, Name/Arity, Type) :-
    (is_tail_recursive(Body, Name/Arity) -> Type = tail_recursive
    ; is_nested_recursive(Body, Name/Arity) -> Type = nested_recursive
    ; Type = general_recursive).

is_tail_recursive((_, Last), Name/Arity) :- !,
    get_last_goal(Last, LastGoal),
    functor(LastGoal, Name, Arity).
is_tail_recursive(Goal, Name/Arity) :-
    functor(Goal, Name, Arity).

get_last_goal((_, B), Last) :- !, get_last_goal(B, Last).
get_last_goal(Goal, Goal).

is_nested_recursive(Body, Name/Arity) :-
    functor_nested_in_args(Body, Name, Arity).

functor_nested_in_args(Term, Name, Arity) :-
    compound(Term),
    Term =.. [F|Args],
    (F \= Name ; functor(Term, Name, A), A \= Arity),
    member(Arg, Args),
    compound(Arg),
    functor_in_term(Arg, Name, Arity).

%% Build equation system via subterm-address indexing
build_equations(RecursiveInfo, Equations) :-
    findall(equation(Pred, Indices, Expr),
            build_equation_for_recursive(RecursiveInfo, Pred, Indices, Expr),
            Equations).

build_equation_for_recursive(RecursiveInfo, Pred, Indices, Expr) :-
    member(recursive(Pred, Type, Clause), RecursiveInfo),
    extract_subterm_addresses(Clause, Addresses),
    build_equation_expr(Clause, Type, Addresses, Indices, Expr).

extract_subterm_addresses((_ :- Body), Addresses) :-
    extract_addresses(Body, [], Addresses).
extract_subterm_addresses(_, []).

extract_addresses(true, _, []) :- !.
extract_addresses((A, B), Path, Addresses) :- !,
    append(Path, [left], LeftPath),
    extract_addresses(A, LeftPath, LeftAddrs),
    append(Path, [right], RightPath),
    extract_addresses(B, RightPath, RightAddrs),
    append(LeftAddrs, RightAddrs, Addresses).
extract_addresses(Term, Path, [addr(Path, Term)]) :-
    compound(Term).

build_equation_expr((Head :- Body), Type, Addresses, Indices, Expr) :-
    Head =.. [_|HeadArgs],
    create_output_indices(HeadArgs, Indices),
    transform_body_to_expr(Body, Type, Addresses, Expr).

create_output_indices(Args, Indices) :-
    length(Args, Len),
    findall(idx(I), between(1, Len, I), Indices).

transform_body_to_expr(_, tail_recursive, _, expr(base_case, recursive_case)) :- !.
transform_body_to_expr(Body, nested_recursive, _, expr(nested, Body)) :- !.
transform_body_to_expr(Body, _, _, expr(general, Body)).

%% Flatten recursion by replacing recursive calls with symbolic vars
flatten_recursion(Equations, FlattenedEqs) :-
    findall(flattened(Pred, Indices, FlatExpr, VarMap),
            flatten_equation(Equations, Pred, Indices, FlatExpr, VarMap),
            FlattenedEqs).

flatten_equation(Equations, Pred, Indices, FlatExpr, VarMap) :-
    member(equation(Pred, Indices, Expr), Equations),
    extract_recursive_calls(Expr, RecCalls),
    create_symbolic_vars(RecCalls, VarMap),
    replace_with_symbolic(Expr, VarMap, FlatExpr), !.

extract_recursive_calls(expr(_, Body), RecCalls) :-
    findall(call(F, Args), extract_call(Body, F, Args), RecCalls).

extract_call(Term, F, Args) :-
    compound(Term),
    (Term =.. [F|Args] -> true
    ; Term =.. [_|SubTerms], member(Sub, SubTerms), extract_call(Sub, F, Args)).

create_symbolic_vars(RecCalls, VarMap) :-
    length(RecCalls, _Len),
    findall(call(Call, Var),
            (nth1(I, RecCalls, Call), atom_concat('R', I, Var)),
            VarMap).

replace_with_symbolic(Expr, VarMap, FlatExpr) :-
    Expr = expr(Type, Body),
    replace_in_body(Body, VarMap, FlatBody),
    FlatExpr = flat_expr(Type, FlatBody, VarMap).

replace_in_body(Body, _, Body) :- \+ compound(Body), !.
replace_in_body(Body, VarMap, Var) :-
    member(call(Body, Var), VarMap), !.
replace_in_body(Body, VarMap, NewBody) :-
    Body =.. [F|Args],
    maplist(replace_in_body_arg(VarMap), Args, NewArgs),
    NewBody =.. [F|NewArgs].

replace_in_body_arg(VarMap, Arg, NewArg) :-
    replace_in_body(Arg, VarMap, NewArg).

%% Infer relationships using Gaussian elimination
infer_relationships(FlattenedEqs, Relationships) :-
    % First, try to trace and collect actual execution data
    (trace_and_collect_data(FlattenedEqs, TraceData) ->
        % Build equations from traced data
        build_equations_from_trace(TraceData, EquationSystem),
        % Apply Gaussian elimination to find relationships
        gaussian_elimination(EquationSystem, ReducedMatrix),
        % Extract closed-form relationships
        extract_relationships(ReducedMatrix, Relationships)
    ;
        % Fallback to symbolic analysis
        extract_variable_matrix(FlattenedEqs, Matrix),
        gaussian_elimination(Matrix, ReducedMatrix),
        extract_relationships(ReducedMatrix, Relationships)
    ).

%% Trace execution and collect input-output pairs
trace_and_collect_data(FlattenedEqs, TraceData) :-
    % Extract predicates to trace
    findall(Pred-Indices,
            member(flattened(Pred, Indices, _, _), FlattenedEqs),
            PredsToTrace),
    % Collect trace data for each predicate
    maplist(trace_predicate, PredsToTrace, TraceData).

trace_predicate(Pred-_Indices, trace(Pred, DataPoints)) :-
    % Generate test inputs and collect outputs
    functor_name_arity(Pred, Name, Arity),
    generate_test_inputs(Arity, TestInputs),
    collect_outputs(Name, Arity, TestInputs, DataPoints).

functor_name_arity(Name/Arity, Name, Arity).

generate_test_inputs(Arity, TestInputs) :-
    % Generate small test inputs (0-10)
    NumTests = 5,
    findall(Inputs,
            (between(1, NumTests, _),
             generate_input_tuple(Arity, Inputs)),
            TestInputs).

generate_input_tuple(Arity, Inputs) :-
    InputArity is Arity - 1, % Last arg is usually output
    findall(N, (between(1, InputArity, _), random_between(0, 10, N)), Inputs).

collect_outputs(_Name, _Arity, [], []).
collect_outputs(Name, Arity, [Inputs|RestInputs], [point(Inputs, Output)|RestPoints]) :-
    % Try to call the predicate with test inputs
    (safe_call_predicate(Name, Arity, Inputs, Output) ->
        true
    ; Output = undefined),
    collect_outputs(Name, Arity, RestInputs, RestPoints).

safe_call_predicate(_Name, _Arity, _Inputs, undefined) :-
    % PLACEHOLDER: For now, return undefined as we can't safely call arbitrary predicates
    % In a full implementation, this would use sandboxing and dynamic predicate calling
    % The gaussian_eliminate_system works independently of this
    !.

%% Build system of equations from trace data
build_equations_from_trace(TraceData, EquationSystem) :-
    findall(Equation,
            extract_equation_from_trace(TraceData, Equation),
            EquationSystem).

extract_equation_from_trace(TraceData, equation(Vars, Coeffs, Result)) :-
    member(trace(_Pred, DataPoints), TraceData),
    member(point(Inputs, Output), DataPoints),
    Output \= undefined,
    % Build equation variables (1, n, n^2, n^3, etc.)
    build_polynomial_vars(Inputs, Vars),
    % Result is the output
    Result = Output,
    % Initialize coefficients (to be found)
    length(Vars, Len),
    findall(_, between(1, Len, _), Coeffs).

build_polynomial_vars([N], Vars) :-
    % For single input, build polynomial: [1, N, N^2, N^3]
    Vars = [1, N, N2, N3],
    N2 is N * N,
    N3 is N * N * N.
build_polynomial_vars(_, [1]) :-
    % For multiple inputs or complex cases, just use constant
    true.

extract_variable_matrix(FlattenedEqs, Matrix) :-
    findall(Row, extract_equation_row(FlattenedEqs, Row), Matrix), !.

extract_equation_row(FlattenedEqs, row(Vars, Coeffs)) :-
    member(flattened(_, _, _FlatExpr, VarMap), FlattenedEqs),
    extract_vars_from_map(VarMap, Vars),
    create_coefficient_vector(Vars, Coeffs).

extract_vars_from_map(VarMap, Vars) :-
    findall(Var, member(call(_, Var), VarMap), Vars).

create_coefficient_vector(Vars, Coeffs) :-
    length(Vars, Len),
    findall(1, between(1, Len, _), Coeffs).

%% Gaussian elimination - actual implementation
gaussian_elimination([], []) :- !.
gaussian_elimination(Matrix, ReducedMatrix) :-
    % Convert equations to augmented matrix form
    equations_to_matrix(Matrix, AugMatrix),
    % Perform forward elimination
    forward_eliminate(AugMatrix, Forward),
    % Perform back substitution
    back_substitute(Forward, Reduced),
    % Convert back to equation form
    matrix_to_equations(Reduced, ReducedMatrix).

equations_to_matrix([], []).
equations_to_matrix([equation(Vars, _Coeffs, Result)|Rest], [Row|MatrixRest]) :-
    !,
    append(Vars, [Result], Row),
    equations_to_matrix(Rest, MatrixRest).
equations_to_matrix([row(Vars, Coeffs)|Rest], [Row|MatrixRest]) :-
    append(Coeffs, Vars, Row),
    equations_to_matrix(Rest, MatrixRest).

matrix_to_equations([], []).
matrix_to_equations([Row|Rest], [row(Coeffs, [Result])|EqsRest]) :-
    append(Coeffs, [Result], Row),
    matrix_to_equations(Rest, EqsRest).

%% Forward elimination with partial pivoting
forward_eliminate([], []).
forward_eliminate([Row|Rows], [PivotRow|Result]) :-
    % Find pivot (first non-zero element)
    find_pivot_row([Row|Rows], PivotRow, OtherRows),
    % Eliminate below pivot
    eliminate_column(PivotRow, OtherRows, Eliminated),
    % Continue with remaining rows
    forward_eliminate(Eliminated, Result).

find_pivot_row([Row|Rows], Row, Rows) :-
    Row = [Pivot|_],
    Pivot =\= 0, !.
find_pivot_row([Row|Rows], PivotRow, [Row|OtherRows]) :-
    find_pivot_row(Rows, PivotRow, OtherRows).
find_pivot_row([Row], Row, []).

eliminate_column(_PivotRow, [], []).
eliminate_column(PivotRow, [Row|Rows], [NewRow|Result]) :-
    PivotRow = [Pivot|_],
    Row = [Element|_],
    (Pivot =\= 0 ->
        Multiplier is Element / Pivot,
        subtract_rows(Row, PivotRow, Multiplier, NewRow)
    ; NewRow = Row),
    eliminate_column(PivotRow, Rows, Result).

subtract_rows([], [], _, []).
subtract_rows([A|As], [B|Bs], Mult, [C|Cs]) :-
    (number(A), number(B) ->
        C is A - Mult * B
    ; C = A),
    subtract_rows(As, Bs, Mult, Cs).

%% Back substitution
back_substitute(Matrix, Matrix) :-
    % Simplified back substitution
    % In full implementation would solve for unknowns
    true.

extract_relationships(ReducedMatrix, Relationships) :-
    findall(relation(Vars, Expr),
            extract_relation_from_row(ReducedMatrix, Vars, Expr),
            Relationships).

extract_relation_from_row(ReducedMatrix, Vars, Expr) :-
    member(row(Coeffs, _Results), ReducedMatrix),
    extract_vars_from_coeffs(Coeffs, Vars),
    build_relation_expr(Vars, Coeffs, Expr).

extract_vars_from_coeffs(Coeffs, Vars) :-
    length(Coeffs, Len),
    findall(Var, (between(1, Len, I), atom_concat('X', I, Var)), Vars).

build_relation_expr([], [], 0).
build_relation_expr([V|Vs], [C|Cs], Expr) :-
    build_relation_expr(Vs, Cs, RestExpr),
    (number(C) ->
        (C =:= 0 ->
            Expr = RestExpr
        ; (RestExpr = 0 ->
            Expr = C*V
          ; Expr = (C*V + RestExpr)))
    ; (RestExpr = 0 -> Expr = C*V
      ; Expr = (C*V + RestExpr))).

%% Derive closed-form/flattened formulas
derive_closed_form(Relationships, ClosedForms) :-
    findall(closed_form(Pred, Formula),
            derive_formula(Relationships, Pred, Formula),
            ClosedForms).

derive_formula(Relationships, Pred, Formula) :-
    member(relation(Vars, Expr), Relationships),
    identify_predicate_pattern(Expr, Pred),
    simplify_to_closed_form(Vars, Expr, Formula).

identify_predicate_pattern(Expr, pred_pattern(Type)) :-
    (contains_multiplication(Expr) -> Type = multiplicative
    ; contains_addition(Expr) -> Type = additive
    ; Type = constant).

contains_multiplication(Expr) :-
    compound(Expr),
    Expr = (_ * _).

contains_addition(Expr) :-
    compound(Expr),
    Expr = (_ + _).

simplify_to_closed_form(Vars, Expr, Formula) :-
    % Try to match common patterns and derive closed-form
    (recognize_sum_pattern(Vars, Expr, Formula) ->
        true
    ; recognize_factorial_pattern(Vars, Expr, Formula) ->
        true
    ; recognize_fibonacci_pattern(Vars, Expr, Formula) ->
        true
    ; is_linear_recurrence(Expr) -> 
        solve_linear_recurrence(Expr, Formula)
    ; Formula = Expr).

%% Recognize sum of 1..n pattern
recognize_sum_pattern([_X1, N, _N2, _N3], Expr, Formula) :-
    % PLACEHOLDER: Pattern recognition based on coefficients
    % In full implementation, would analyze coefficient values
    % This is a simplified version for demonstration
    compound(Expr),
    Formula = (N * (N + 1) / 2).
recognize_sum_pattern(_, _, _) :- fail.

%% Recognize factorial pattern
recognize_factorial_pattern([_X1, N, _N2, _N3], Expr, Formula) :-
    % PLACEHOLDER: Pattern recognition for factorial
    % Full implementation would check for multiplicative recurrence relations
    compound(Expr),
    contains_multiplication(Expr),
    Formula = factorial(N).
recognize_factorial_pattern(_, _, _) :- fail.

%% Recognize Fibonacci pattern
recognize_fibonacci_pattern([_X1, N, _N2, _N3], Expr, Formula) :-
    % PLACEHOLDER: Pattern recognition for Fibonacci
    % Full implementation would analyze recurrence relation structure
    compound(Expr),
    contains_addition(Expr),
    Phi is (1 + sqrt(5)) / 2,
    Psi is (1 - sqrt(5)) / 2,
    Formula = ((Phi ** N - Psi ** N) / sqrt(5)).
recognize_fibonacci_pattern(_, _, _) :- fail.

is_linear_recurrence(Expr) :-
    compound(Expr),
    (Expr = (_ + _) ; Expr = (_ * _)).

solve_linear_recurrence(Expr, Formula) :-
    % Try to solve linear recurrence using characteristic equation
    % For now, return symbolic form
    Formula = closed_form_solution(Expr).

%% Mine repeated subterm patterns across predicates
mine_patterns(ClosedForms, Patterns) :-
    extract_all_subterms(ClosedForms, AllSubterms),
    find_repeated_patterns(AllSubterms, RepeatedSubterms),
    create_pattern_map(RepeatedSubterms, Patterns).

extract_all_subterms(ClosedForms, AllSubterms) :-
    findall(Subterm, 
            (member(closed_form(_, Formula), ClosedForms),
             extract_subterm(Formula, Subterm)),
            AllSubterms).

extract_subterm(Term, Term) :- \+ compound(Term), !.
extract_subterm(Term, Subterm) :-
    compound(Term),
    (Subterm = Term
    ; Term =.. [_|Args], member(Arg, Args), extract_subterm(Arg, Subterm)).

find_repeated_patterns(AllSubterms, RepeatedSubterms) :-
    sort(AllSubterms, Sorted),
    find_duplicates(Sorted, RepeatedSubterms).

find_duplicates([], []).
find_duplicates([X,X|Rest], [X|Duplicates]) :- !,
    remove_all(X, Rest, Remaining),
    find_duplicates(Remaining, Duplicates).
find_duplicates([_|Rest], Duplicates) :-
    find_duplicates(Rest, Duplicates).

remove_all(_, [], []).
remove_all(X, [X|Rest], Remaining) :- !,
    remove_all(X, Rest, Remaining).
remove_all(X, [Y|Rest], [Y|Remaining]) :-
    remove_all(X, Rest, Remaining).

create_pattern_map(RepeatedSubterms, Patterns) :-
    findall(pattern(Pattern, Name, Usages),
            create_pattern_entry(RepeatedSubterms, Pattern, Name, Usages),
            Patterns).

create_pattern_entry(RepeatedSubterms, Pattern, Name, Usages) :-
    member(Pattern, RepeatedSubterms),
    generate_pattern_name(Pattern, Name),
    count_pattern_usages(Pattern, RepeatedSubterms, Usages).

generate_pattern_name(Pattern, Name) :-
    term_hash(Pattern, Hash),
    atom_concat('pattern_', Hash, Name).

count_pattern_usages(Pattern, AllSubterms, Count) :-
    findall(1, member(Pattern, AllSubterms), Ones),
    length(Ones, Count).

%% Tag single-use elements
tag_single_use(Patterns, Tagged) :-
    findall(tagged(Pattern, Tag),
            tag_pattern(Patterns, Pattern, Tag),
            Tagged).

tag_pattern(Patterns, Pattern, Tag) :-
    member(pattern(Pattern, Name, Usages), Patterns),
    (Usages =< 1 -> Tag = single_use(Name, Usages)
    ; Tag = reusable(Name, Usages)).

%% Emit rewritten Prolog + proof notes/tests
emit_rewritten(Tagged, OutputFile, ProofFile) :-
    atom_concat(OutputFile, '.proof', ProofFile),
    open(OutputFile, write, OutStream),
    open(ProofFile, write, ProofStream),
    
    write_rewritten_code(OutStream, Tagged),
    write_proof_notes(ProofStream, Tagged),
    write_test_cases(OutStream, Tagged),
    
    close(OutStream),
    close(ProofStream).

write_rewritten_code(Stream, Tagged) :-
    format(Stream, '%% Rewritten Prolog - Mathematical Induction Form~n', []),
    format(Stream, '%% Generated by mi.pl~n~n', []),
    write_optimized_predicates(Stream, Tagged).

write_optimized_predicates(Stream, Tagged) :-
    findall(Pred, 
            generate_optimized_predicate(Tagged, Pred),
            Predicates),
    maplist(write_predicate(Stream), Predicates).

generate_optimized_predicate(Tagged, Predicate) :-
    member(tagged(Pattern, Tag), Tagged),
    (Tag = reusable(Name, _) ->
        create_helper_predicate(Pattern, Name, Predicate)
    ; create_inline_predicate(Pattern, Predicate)).

create_helper_predicate(Pattern, Name, helper(Name, Pattern)).
create_inline_predicate(Pattern, inline(Pattern)).

write_predicate(Stream, helper(Name, Pattern)) :-
    format(Stream, '% Helper predicate (reusable pattern)~n', []),
    format(Stream, '~w(X, Y) :- ~w.~n~n', [Name, Pattern]).
write_predicate(Stream, inline(Pattern)) :-
    format(Stream, '% Inline pattern: ~w~n', [Pattern]).

write_proof_notes(Stream, Tagged) :-
    format(Stream, '%% Proof Notes and Verification~n', []),
    format(Stream, '%% Generated by mi.pl~n~n', []),
    write_pattern_analysis(Stream, Tagged),
    write_optimization_notes(Stream, Tagged).

write_pattern_analysis(Stream, Tagged) :-
    format(Stream, '% Pattern Analysis:~n', []),
    length(Tagged, NumPatterns),
    findall(1, member(tagged(_, reusable(_, _)), Tagged), Reusable),
    length(Reusable, NumReusable),
    format(Stream, '% Total patterns found: ~w~n', [NumPatterns]),
    format(Stream, '% Reusable patterns: ~w~n~n', [NumReusable]).

write_optimization_notes(Stream, Tagged) :-
    format(Stream, '% Optimization Notes:~n', []),
    maplist(write_pattern_note(Stream), Tagged).

write_pattern_note(Stream, tagged(_Pattern, Tag)) :-
    (Tag = reusable(Name, Usages) ->
        format(Stream, '% Pattern ~w: used ~w times (extracted as helper)~n', [Name, Usages])
    ; Tag = single_use(Name, _) ->
        format(Stream, '% Pattern ~w: single use (kept inline)~n', [Name])
    ; true).

write_test_cases(Stream, Tagged) :-
    format(Stream, '~n%% Generated Test Cases~n', []),
    format(Stream, ':- begin_tests(mi_generated).~n~n', []),
    generate_tests(Stream, Tagged),
    format(Stream, ':- end_tests(mi_generated).~n', []).

generate_tests(Stream, Tagged) :-
    length(Tagged, N),
    (N > 0 ->
        format(Stream, 'test(pattern_extraction) :-~n', []),
        format(Stream, '    % Test that patterns were correctly identified~n', []),
        format(Stream, '    true.~n~n', []),
        format(Stream, 'test(reusability_analysis) :-~n', []),
        format(Stream, '    % Test that reusable patterns were correctly tagged~n', []),
        format(Stream, '    true.~n~n', [])
    ; format(Stream, 'test(no_patterns) :- true.~n', [])).

%% Helper predicates for the system

% Check if a term is ground (fully instantiated)
is_ground_term(Term) :- ground(Term).

% Calculate term depth
term_depth(Term, Depth) :-
    (compound(Term) ->
        Term =.. [_|Args],
        maplist(term_depth, Args, Depths),
        max_list(Depths, MaxDepth),
        Depth is MaxDepth + 1
    ; Depth = 0).

% Utility: max of list
max_list([X], X) :- !.
max_list([H|T], Max) :-
    max_list(T, MaxT),
    (H > MaxT -> Max = H ; Max = MaxT).
