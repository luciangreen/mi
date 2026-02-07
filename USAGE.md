# Usage Guide for MI - Mathematical Induction Converter

## Quick Start

### Loading the Module

```prolog
?- [mi].
true.
```

### Basic Usage

#### 1. Analyze Prolog Clauses Programmatically

```prolog
?- use_module(mi).
?- Clauses = [(factorial(0, 1)), 
              (factorial(N, R) :- N > 0, factorial(N-1, R1), R is N*R1)],
   analyze_predicates(Clauses, Result).
```

#### 2. Detect Recursive Predicates

```prolog
?- Clauses = [(fib(0,0)), (fib(1,1)), 
              (fib(N,R) :- N>1, fib(N-1,F1), fib(N-2,F2), R is F1+F2)],
   detect_recursive(Clauses, RecInfo).

RecInfo = [recursive(fib/2, nested_recursive, ...)].
```

#### 3. Process a File

```prolog
?- process_file('examples/factorial.pl', 'output.pl').
```

This will:
- Parse all clauses from `factorial.pl`
- Detect recursive predicates
- Build equation systems
- Flatten recursion with symbolic variables
- Infer relationships via Gaussian elimination
- Derive closed-form solutions
- Mine reusable patterns
- Tag single-use elements
- Generate `output.pl` with optimized code
- Generate `output.pl.proof` with proof notes and tests

## Advanced Usage

### Pipeline Components

You can use individual components of the pipeline:

```prolog
% Parse clauses
?- parse_clauses([foo(X) :- bar(X)], Parsed).

% Build equations from recursive info
?- detect_recursive(Clauses, RecInfo),
   build_equations(RecInfo, Equations).

% Flatten recursion
?- flatten_recursion(Equations, FlattenedEqs).

% Mine patterns
?- mine_patterns(ClosedForms, Patterns).

% Tag for reuse
?- tag_single_use(Patterns, Tagged).
```

### Understanding Output

#### Recursive Detection Types

- `tail_recursive` - Last call is recursive (optimizable)
- `nested_recursive` - Multiple recursive calls (e.g., Fibonacci)
- `general_recursive` - Other recursive patterns

#### Pattern Tags

- `single_use(Name, 1)` - Pattern appears once, kept inline
- `reusable(Name, N)` - Pattern appears N times, extracted as helper

### Example: Complete Analysis

```prolog
?- use_module(mi).

% Define recursive predicates
?- Clauses = [
       (len([], 0)),
       (len([_|T], N) :- len(T, N1), N is N1 + 1)
   ],
   
   % Detect recursion
   detect_recursive(Clauses, RecInfo),
   format('Found: ~w~n', [RecInfo]),
   
   % Build equations
   build_equations(RecInfo, Equations),
   format('Equations: ~w~n', [Equations]),
   
   % Continue pipeline...
   analyze_predicates(Clauses, Result),
   format('Final result: ~w~n', [Result]).
```

## Running Demonstrations

A demo script is provided to showcase features:

```prolog
?- [demo].
?- run_all_demos.
```

Individual demos:
```prolog
?- demo_basic.          % Basic recursion detection
?- demo_pipeline.       % Full analysis pipeline
?- demo_patterns.       % Pattern mining
?- demo_file_processing. % File I/O
```

## Testing

Run the test suite:

```prolog
?- [tests/test_mi].
?- run_all_tests.
```

Tests cover:
- Clause parsing
- Recursion detection (tail, nested, general)
- Equation building with subterm indexing
- Recursion flattening with symbolic variables
- Pattern mining and frequency analysis
- Single-use tagging
- Integration tests
- Utility functions

## Examples

See `examples/factorial.pl` for sample recursive predicates:
- `factorial/2` - Simple recursion
- `fibonacci/2` - Nested recursion (two recursive calls)
- `list_length/2` - Tail recursion
- `sum_list/3` - Accumulator pattern

## Typical Workflow

1. **Write or load recursive Prolog code**
   ```prolog
   % your_code.pl
   factorial(0, 1).
   factorial(N, R) :- N > 0, factorial(N-1, R1), R is N*R1.
   ```

2. **Process with MI**
   ```prolog
   ?- process_file('your_code.pl', 'optimized.pl').
   ```

3. **Review outputs**
   - `optimized.pl` - Rewritten code with extracted helpers
   - `optimized.pl.proof` - Proof notes and analysis

4. **Examine proof notes**
   - Pattern analysis (how many patterns found)
   - Reusability analysis (what was extracted)
   - Generated test cases

5. **Use optimized code**
   The output can be loaded and used like normal Prolog:
   ```prolog
   ?- [optimized].
   ```

## Tips

- **Start simple**: Test with small recursive predicates first
- **Check proof notes**: Review the `.proof` file for insights
- **Understand patterns**: Reusable patterns become helper predicates
- **Validate output**: Generated tests help verify correctness
- **Iterate**: Refine your recursive predicates based on analysis

## Troubleshooting

### Module won't load
```
Error: mi.pl not found
```
**Solution**: Ensure you're in the correct directory or use full path:
```prolog
?- use_module('/full/path/to/mi').
```

### No patterns found
If pattern mining returns empty results, this is normal for:
- Simple predicates with unique formulas
- Non-recursive code
- Single predicate analysis

### File processing fails
Ensure:
- Input file exists and is readable
- Output directory is writable
- Input file contains valid Prolog syntax
- Terms end with periods (`.`)

## API Reference Summary

| Predicate | Purpose |
|-----------|---------|
| `process_file/2` | Main entry: file in → optimized out |
| `analyze_predicates/2` | Programmatic: clauses → analysis |
| `detect_recursive/2` | Find recursive predicates |
| `build_equations/2` | Create equation system |
| `flatten_recursion/2` | Replace calls with variables |
| `infer_relationships/2` | Gaussian elimination |
| `derive_closed_form/2` | Get closed-form solutions |
| `mine_patterns/2` | Find repeated subterms |
| `tag_single_use/2` | Tag reusable vs inline |
| `emit_rewritten/3` | Generate output files |

## Further Reading

- SWI-Prolog documentation: https://www.swi-prolog.org/
- Prolog recursion patterns
- Mathematical induction in program analysis
- Symbolic computation and term rewriting
