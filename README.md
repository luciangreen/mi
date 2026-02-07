# MI - Mathematical Induction Converter for Prolog

Converts nested recursive Prolog predicates to mathematical induction form through advanced static analysis and symbolic computation.

## Overview

This tool analyzes Prolog programs containing recursive predicates and transforms them into optimized mathematical induction forms. It performs:

1. **Clause Parsing** - Reads and structures Prolog clauses
2. **Recursion Detection** - Identifies nested, tail-recursive, and general recursive patterns
3. **Execution Tracing** - Collects input-output pairs from recursive predicates
4. **Equation System Building** - Constructs equation systems from trace data
5. **Gaussian Elimination** - Applies real Gaussian elimination to solve for coefficients
6. **Closed-Form Derivation** - Produces mathematical formulas (e.g., sum(1..n) = n*(n+1)/2)
7. **Pattern Mining** - Discovers repeated subterm patterns for code reuse
8. **Single-Use Tagging** - Identifies and tags single-use vs. reusable elements
9. **Code Generation** - Emits optimized Prolog with proof notes and tests

## Installation

1. Ensure you have SWI-Prolog installed (7.0 or higher)
2. Clone this repository:
   ```bash
   git clone https://github.com/luciangreen/mi.git
   cd mi
   ```

## Usage

### Command Line

```prolog
?- [mi].
?- process_file('examples/factorial.pl', 'output.pl').
```

### Programmatic API

```prolog
?- use_module(mi).

% Analyze clauses without file I/O
?- analyze_predicates([
       (factorial(0, 1)),
       (factorial(N, R) :- N > 0, factorial(N-1, R1), R is N*R1)
   ], Result).

% Parse and analyze specific clauses
?- parse_clauses([(foo(X) :- bar(X))], Parsed).
?- detect_recursive(Clauses, RecursiveInfo).
?- build_equations(RecursiveInfo, Equations).
```

### Example Input

```prolog
% factorial.pl
factorial(0, 1).
factorial(N, Result) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, R1),
    Result is N * R1.
```

### Example Output

The tool generates:
- **Optimized Prolog code** - Rewritten predicates with extracted helper functions
- **Proof notes** - Analysis of patterns, optimization decisions
- **Test cases** - Validation tests for the transformations

## Features

### Recursion Analysis

- **Tail Recursion Detection** - Identifies tail-recursive predicates
- **Nested Recursion Detection** - Finds multiply-recursive predicates (e.g., Fibonacci)
- **General Recursion Classification** - Categorizes other recursive patterns

### Equation System Building

- **Subterm-Address Indexing** - Precisely tracks term positions
- **Output Variable Tracking** - Maintains output relationships
- **Pattern Extraction** - Builds symbolic equations from recursive definitions

### Symbolic Computation

- **Execution Tracing** - Collects actual input-output pairs from recursive predicates
- **Equation Building** - Constructs polynomial equation systems from trace data
- **Gaussian Elimination** - Real numerical Gaussian elimination with forward elimination and back substitution
- **Closed-Form Solutions** - Derives mathematical formulas from equations
  - Example: sum(1..n) = 0.5*n + 0.5*n^2 = n*(n+1)/2
  - Example: sum(1^2..n^2) = n/6 + n^2/2 + n^3/3 = n*(n+1)*(2n+1)/6

### Nested Recursion Handling

- **Multiple Branch Analysis** - Traces each recursive call separately
- **Branch Merging** - Combines formulas from multiple recursive branches
- **Pattern Recognition** - Identifies exponential patterns (e.g., Fibonacci)
- **Formula Derivation** - Produces single closed-form for nested recursion

### Pattern Mining

- **Subterm Analysis** - Extracts all subterms from formulas
- **Frequency Analysis** - Counts pattern occurrences
- **Reuse Detection** - Identifies opportunities for helper predicates
- **Single-Use Tagging** - Marks inline-only patterns

## API Reference

### Main Predicates

#### `process_file(+InputFile, -OutputFile)`
Process a Prolog source file and generate optimized output.

#### `analyze_predicates(+Clauses, -Result)`
Analyze a list of clauses programmatically.

#### `parse_clauses(+Terms, -ParsedClauses)`
Parse Prolog terms into structured format.

#### `detect_recursive(+Clauses, -RecursiveInfo)`
Detect recursive predicates and classify their type.

#### `build_equations(+RecursiveInfo, -Equations)`
Build equation system from recursive definitions.

#### `flatten_recursion(+Equations, -FlattenedEqs)`
Flatten recursion by introducing symbolic variables.

#### `infer_relationships(+FlattenedEqs, -Relationships)`
Infer variable relationships using Gaussian elimination.

#### `derive_closed_form(+Relationships, -ClosedForms)`
Derive closed-form formulas from relationships.

#### `mine_patterns(+ClosedForms, -Patterns)`
Mine repeated subterm patterns.

#### `tag_single_use(+Patterns, -Tagged)`
Tag patterns as single-use or reusable.

#### `emit_rewritten(+Tagged, +OutputFile, -ProofFile)`
Generate optimized code and proof notes.

#### `flatten_to_formula(+PredicateName, +Arity, +TestCases, -ClosedFormFormula)`
Flatten a recursive predicate into a closed-form mathematical formula by:
1. Tracing execution with test cases
2. Building polynomial equation system
3. Applying Gaussian elimination
4. Deriving closed-form formula

Example:
```prolog
?- flatten_to_formula(sum_to, 2, [[0], [1], [2], [3], [4]], Formula).
Formula = 0.5*n + 0.5*n^2.  % Simplified: n*(n+1)/2
```

#### `gaussian_eliminate_system(+EquationSystem, -Solutions)`
Apply Gaussian elimination to a system of linear equations to find coefficient values.

Example:
```prolog
?- gaussian_eliminate_system([
       equation([1, 1], 3),    % x + y = 3
       equation([2, 1], 5)     % 2x + y = 5
   ], Solutions).
Solutions = [2, 1].  % x=2, y=1
```

## Examples

See the `examples/` directory for sample Prolog programs:

- `factorial.pl` - Factorial, Fibonacci, list operations
- `sum_example.pl` - Demonstrates deriving closed-form formula for sum(1..n) using Gaussian elimination
- `fibonacci_example.pl` - Shows handling of nested recursion (Fibonacci sequence)
- `simple_example.pl` - Basic examples

### Example: Deriving Formula for Sum

```prolog
?- [examples/sum_example].
?- demo_manual_gaussian.

=== Manual Gaussian Elimination Demo ===

Equation system (polynomial form a0 + a1*n + a2*n^2 + a3*n^3):
equation([1,0,0,0],0)
equation([1,1,1,1],1)
equation([1,2,4,8],3)
equation([1,3,9,27],6)

Applying Gaussian elimination...
Solutions (coefficients):
[0.0,0.5,0.5,0.0]

a0 = 0.0
a1 = 0.5
a2 = 0.5
a3 = 0.0

Formula: 0.0 + 0.5*n + 0.5*n^2 + 0.0*n^3

Simplified: 0.5*n^2 + 0.5*n = n*(n+1)/2
This matches the known formula for sum of 1..n!
```

### Example: Nested Recursion (Fibonacci)

```prolog
?- [examples/fibonacci_example].
?- demo_nested_flattening.

=== Flattening Nested Recursion Step-by-Step ===

Original: fib(n) = fib(n-1) + fib(n-2)

Step 1: Trace recursive calls
Step 2: Build equations from outputs
Step 3: Find pattern using Gaussian elimination
Step 4: Handle each recursive branch
Step 5: Merge branches into single formula
```

## Testing

Run the test suite:

```prolog
?- [tests/test_mi].
?- run_all_tests.
```

Test coverage includes:
- Clause parsing
- Recursion detection (tail, nested, general)
- Equation building
- Flattening
- **Gaussian elimination** (2x2 systems, polynomial formulas, identity matrices)
- **Formula derivation** (sum of integers, sum of squares)
- Pattern mining
- Tagging
- Integration tests
- Utility functions

Run Gaussian elimination tests specifically:

```prolog
?- [tests/test_gaussian].
?- run_gaussian_tests.
% All 6 tests passed
```

## Implementation Details

### Subterm-Address Indexing

Uses path-based addressing to precisely locate subterms:
```
Term: f(g(a), h(b))
Addresses: [left, left] -> g, [left, left, left] -> a
           [right] -> h, [right, left] -> b
```

### Gaussian Elimination

**Implementation**: Real numerical Gaussian elimination with forward elimination and back substitution.

**Algorithm**:
1. **Build Augmented Matrix**: Convert equation system to matrix form [A | b]
2. **Forward Elimination**: Reduce matrix to row echelon form
   - Normalize pivot rows by dividing by pivot element
   - Eliminate coefficients below each pivot
3. **Back Substitution**: Solve from bottom to top
   - Start with last row (simplest equation)
   - Substitute known values into previous rows
   - Calculate each variable sequentially

**Example** - Solving for sum(1..n):
```
Input data points: (0,0), (1,1), (2,3), (3,6)
Polynomial form: a0 + a1*n + a2*n^2 + a3*n^3 = output

Equations:
  a0                                  = 0
  a0 + a1    + a2    + a3    = 1
  a0 + 2*a1  + 4*a2  + 8*a3  = 3
  a0 + 3*a1  + 9*a2  + 27*a3 = 6

Augmented matrix:
  [1  0  0  0  | 0]
  [1  1  1  1  | 1]
  [1  2  4  8  | 3]
  [1  3  9  27 | 6]

After Gaussian elimination:
  [1  0  0  0  | 0  ]
  [0  1  1  1  | 1  ]
  [0  0  1  3  | 0.5]
  [0  0  0  1  | 0  ]

Back substitution:
  a3 = 0
  a2 + 3*0 = 0.5  =>  a2 = 0.5
  a1 + 0.5 + 0 = 1  =>  a1 = 0.5  
  a0 = 0

Result: 0 + 0.5*n + 0.5*n^2 = n*(n+1)/2 ✓
```

### Pattern Mining Algorithm

1. Extract all subterms from closed-form solutions
2. Sort and identify duplicates
3. Count usage frequency
4. Tag based on reuse potential
5. Generate helper predicates for reusable patterns

## Limitations

- Polynomial approximation works best for formulas that can be expressed as polynomials
- Exponential patterns (like Fibonacci) require different approach than polynomials
- Nested recursion handling is demonstrated but full merging requires additional work
- File I/O requires valid Prolog syntax
- Tracing requires predicates to be defined and executable in current context

## Contributing

Contributions welcome! Areas for enhancement:
- Enhanced symbolic solver
- More recursion pattern detection
- Optimization heuristics
- Additional test cases
- Performance improvements

## License

See LICENSE file for details.

## Author

Lucian Green

## See Also

- [SWI-Prolog Documentation](https://www.swi-prolog.org/)
- Mathematical Induction in Program Analysis
- Symbolic Computation Techniques
