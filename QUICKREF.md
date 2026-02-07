# MI Quick Reference

## Load Module
```prolog
?- use_module(mi).
```

## Main Commands

### Process a File
```prolog
?- process_file('input.pl', 'output.pl').
```
Generates:
- `output.pl` - Optimized code
- `output.pl.proof` - Proof notes

### Analyze Clauses
```prolog
?- Clauses = [(factorial(0,1)), (factorial(N,R) :- ...)],
   analyze_predicates(Clauses, Result).
```

### Detect Recursion Only
```prolog
?- detect_recursive(Clauses, RecInfo).
RecInfo = [recursive(Name/Arity, Type, Clause), ...]
```

## Pipeline Components

| Step | Predicate | Input | Output |
|------|-----------|-------|--------|
| 1 | `parse_clauses/2` | Terms | Parsed structures |
| 2 | `detect_recursive/2` | Clauses | Recursive info |
| 3 | `build_equations/2` | Rec info | Equations |
| 4 | `flatten_recursion/2` | Equations | Flattened |
| 5 | `infer_relationships/2` | Flattened | Relationships |
| 6 | `derive_closed_form/2` | Relations | Closed forms |
| 7 | `mine_patterns/2` | Closed forms | Patterns |
| 8 | `tag_single_use/2` | Patterns | Tagged |
| 9 | `emit_rewritten/3` | Tagged | Files |

## Recursion Types

- `tail_recursive` - Optimizable tail calls
- `nested_recursive` - Multiple recursive calls (e.g., Fibonacci)
- `general_recursive` - Other patterns

## Pattern Tags

- `single_use(Name, 1)` - Kept inline
- `reusable(Name, N)` - Extracted as helper (used N times)

## Examples

### Basic Factorial
```prolog
?- Clauses = [
       (factorial(0, 1)),
       (factorial(N, R) :- N > 0, factorial(N-1, R1), R is N*R1)
   ],
   detect_recursive(Clauses, Info).
```

### Fibonacci
```prolog
?- Clauses = [
       (fib(0, 0)),
       (fib(1, 1)),
       (fib(N, R) :- N > 1, fib(N-1, F1), fib(N-2, F2), R is F1+F2)
   ],
   analyze_predicates(Clauses, Result).
```

### List Length
```prolog
?- Clauses = [
       (len([], 0)),
       (len([_|T], N) :- len(T, N1), N is N1+1)
   ],
   process_file('list.pl', 'list_opt.pl').
```

## Testing

### Run All Tests
```prolog
?- [tests/test_mi].
?- run_all_tests.
% All 19 tests passed
```

### Run Demos
```prolog
?- [demo].
?- run_all_demos.
```

### Quick Test
```prolog
?- [demo].
?- quick_test.
✓ Analysis complete.
```

## Utilities

### Term Depth
```prolog
?- term_depth(f(g(h(x))), D).
D = 3.
```

## File Structure

```
input.pl          → Your Prolog source
  ↓ process_file
output.pl         → Optimized code
output.pl.proof   → Analysis notes
```

## Common Patterns

### Single Predicate Analysis
```prolog
analyze_one(Predicate, Result) :-
    detect_recursive([Predicate], RecInfo),
    build_equations(RecInfo, Eqs),
    flatten_recursion(Eqs, Result).
```

### Custom Pipeline
```prolog
my_pipeline(Input, Output) :-
    parse_clauses(Input, Parsed),
    detect_recursive(Parsed, Rec),
    % ... customize pipeline ...
    emit_rewritten(Result, 'output.pl', _).
```

## Troubleshooting

### No Recursion Detected
- Check if predicates are actually recursive
- Verify clause structure

### No Patterns Found
- Normal for unique formulas
- Try more predicates

### File Not Found
```prolog
?- exists_file('path/to/file.pl').
```

## More Help

- Full documentation: `README.md`
- Usage guide: `USAGE.md`
- Architecture: `ARCHITECTURE.md`
- Examples: `examples/`
- Tests: `tests/test_mi.pl`

## Version
MI v1.0.0 - Mathematical Induction Converter for Prolog
