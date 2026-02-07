# MI Architecture and Design

## System Overview

The MI (Mathematical Induction) converter is a sophisticated Prolog program analysis tool that transforms recursive predicates into mathematical induction form through a multi-stage pipeline.

## Architecture

```
Input Prolog File
       ↓
┌──────────────────────────────────────────┐
│  1. PARSING STAGE                        │
│  - Read Prolog clauses                   │
│  - Structure terms                       │
│  - Extract metadata                      │
└──────────────────────────────────────────┘
       ↓
┌──────────────────────────────────────────┐
│  2. DETECTION STAGE                      │
│  - Identify recursive predicates         │
│  - Classify recursion type:              │
│    • Tail recursive                      │
│    • Nested recursive                    │
│    • General recursive                   │
└──────────────────────────────────────────┘
       ↓
┌──────────────────────────────────────────┐
│  3. EQUATION BUILDING STAGE              │
│  - Extract subterm addresses             │
│  - Build output indices                  │
│  - Create equation expressions           │
└──────────────────────────────────────────┘
       ↓
┌──────────────────────────────────────────┐
│  4. FLATTENING STAGE                     │
│  - Extract recursive calls               │
│  - Create symbolic variables (R1, R2...) │
│  - Replace calls with symbols            │
└──────────────────────────────────────────┘
       ↓
┌──────────────────────────────────────────┐
│  5. INFERENCE STAGE                      │
│  - Build variable matrix                 │
│  - Apply Gaussian elimination            │
│  - Extract relationships                 │
└──────────────────────────────────────────┘
       ↓
┌──────────────────────────────────────────┐
│  6. CLOSED-FORM DERIVATION               │
│  - Identify patterns (additive, etc.)    │
│  - Solve linear recurrences              │
│  - Generate formulas                     │
└──────────────────────────────────────────┘
       ↓
┌──────────────────────────────────────────┐
│  7. PATTERN MINING STAGE                 │
│  - Extract all subterms                  │
│  - Find repeated patterns                │
│  - Count usage frequency                 │
│  - Create pattern map                    │
└──────────────────────────────────────────┘
       ↓
┌──────────────────────────────────────────┐
│  8. TAGGING STAGE                        │
│  - Tag single-use patterns               │
│  - Tag reusable patterns                 │
│  - Generate helper names                 │
└──────────────────────────────────────────┘
       ↓
┌──────────────────────────────────────────┐
│  9. EMISSION STAGE                       │
│  - Generate optimized Prolog             │
│  - Create helper predicates              │
│  - Write proof notes                     │
│  - Generate test cases                   │
└──────────────────────────────────────────┘
       ↓
  Output Files:
  - *.pl (optimized code)
  - *.pl.proof (proof notes)
```

## Key Algorithms

### 1. Subterm-Address Indexing

**Purpose**: Precisely locate subterms within complex term structures.

**Algorithm**:
```
For term T at position P:
  - If T is atomic: address = P
  - If T = f(A₁, ..., Aₙ):
    - Address of f = P
    - Address of Aᵢ = P ∪ {position_i}
```

**Example**:
```prolog
Term: factorial(N, R) :- factorial(N-1, R1), R is N*R1

Addresses:
  [left] → factorial(N-1, R1)
  [left, left] → N-1
  [right] → R is N*R1
  [right, left] → N*R1
```

### 2. Recursion Detection

**Classification Rules**:

1. **Tail Recursive**: Last goal in body is recursive call
   ```prolog
   list_sum([H|T], Acc, Sum) :-
       NewAcc is Acc + H,
       list_sum(T, NewAcc, Sum).  % ← tail position
   ```

2. **Nested Recursive**: Multiple recursive calls
   ```prolog
   fib(N, R) :-
       fib(N-1, R1),  % ← recursive call 1
       fib(N-2, R2),  % ← recursive call 2
       R is R1 + R2.
   ```

3. **General Recursive**: Other patterns
   ```prolog
   factorial(N, R) :-
       factorial(N-1, R1),
       R is N * R1.
   ```

### 3. Symbolic Flattening

**Purpose**: Convert recursive calls to symbolic variables for analysis.

**Algorithm**:
```
Input: factorial(N, R) :- factorial(N-1, R1), R is N*R1

Step 1: Identify recursive calls
  Calls = [factorial(N-1, R1)]

Step 2: Create symbolic variables
  VarMap = [call(factorial(N-1, R1), 'R1')]

Step 3: Substitute
  R is N * R1  (where R1 is now symbolic)
```

### 4. Gaussian Elimination (Simplified)

**Purpose**: Solve system of symbolic equations.

**Current Implementation**: Simplified for symbolic terms
- Builds coefficient matrix
- Performs forward elimination
- Prepares for back substitution

**Future Enhancement**: Full symbolic row operations

### 5. Pattern Mining

**Algorithm**:
```
1. Extract all subterms from formulas:
   flatten_term(T) = {T} ∪ ⋃ flatten_term(subterm(T))

2. Sort and find duplicates:
   duplicates(L) = {x ∈ L | count(x) > 1}

3. Count frequencies:
   freq(pattern) = |{formula | pattern ⊆ formula}|

4. Generate names:
   name(pattern) = hash(pattern) → "pattern_123"

5. Tag by usage:
   if freq(p) ≤ 1: single_use
   else: reusable
```

## Data Structures

### Internal Representations

1. **Parsed Clause**:
   ```prolog
   parsed(Head, Body, Metadata)
   ```

2. **Recursive Info**:
   ```prolog
   recursive(Predicate, Type, Clause)
   where Type ∈ {tail_recursive, nested_recursive, general_recursive}
   ```

3. **Equation**:
   ```prolog
   equation(Predicate, OutputIndices, Expression)
   ```

4. **Flattened Equation**:
   ```prolog
   flattened(Predicate, Indices, FlatExpr, VarMap)
   ```

5. **Pattern**:
   ```prolog
   pattern(SubTerm, GeneratedName, UsageCount)
   ```

6. **Tagged Pattern**:
   ```prolog
   tagged(Pattern, Tag)
   where Tag ∈ {single_use(Name, Count), reusable(Name, Count)}
   ```

## Design Decisions

### Why Modular Pipeline?

Each stage is independent and testable:
- Can run individual stages for debugging
- Can inject data between stages
- Can test each transformation separately

### Why Symbolic Variables?

Allows mathematical analysis:
- Can apply algebraic transformations
- Can detect linear relationships
- Can derive closed-form solutions

### Why Pattern Mining?

Enables code optimization:
- Extracts common subexpressions
- Creates reusable helper predicates
- Reduces code duplication

### Why Proof Notes?

Provides transparency:
- Shows analysis decisions
- Validates transformations
- Helps debugging
- Documents optimizations

## Extension Points

### Adding New Recursion Types

1. Add detector in `classify_recursion_type/3`
2. Add equation builder in `build_equation_expr/5`
3. Add closed-form solver in `solve_linear_recurrence/2`

### Adding New Pattern Types

1. Extend `extract_subterm/2` for special forms
2. Add pattern classifiers in `identify_predicate_pattern/2`
3. Add simplifiers in `simplify_to_closed_form/2`

### Adding New Optimizations

1. Extend `tag_single_use/2` with new criteria
2. Add optimization passes in `emit_rewritten/3`
3. Add proof note generators

## Performance Characteristics

### Time Complexity

- **Parsing**: O(n) where n = number of clauses
- **Detection**: O(n × m) where m = average clause size
- **Equation Building**: O(n × d) where d = term depth
- **Pattern Mining**: O(p²) where p = number of patterns
- **Overall**: O(n × m × d + p²)

### Space Complexity

- **Storage**: O(n × d) for term structures
- **Pattern Map**: O(p) unique patterns
- **Matrix**: O(r × c) for r recursive predicates, c variables

## Testing Strategy

### Unit Tests
- Individual predicates
- Edge cases
- Singleton variables
- Module exports

### Integration Tests
- Full pipeline
- File I/O
- Multi-clause analysis

### Example-Based Tests
- factorial
- fibonacci
- list operations
- accumulator patterns

## Future Enhancements

1. **Enhanced Solver**
   - Complete Gaussian elimination with symbolic math
   - Non-linear recurrence solving
   - Closed-form for more patterns

2. **Advanced Pattern Detection**
   - Structural patterns (fold, map, filter)
   - Tail-call optimization opportunities
   - Accumulator introduction

3. **Optimization Heuristics**
   - Cost-based optimization
   - Profiling-guided optimization
   - Memory usage analysis

4. **Better Output**
   - Formatted Prolog with comments
   - Visual proofs
   - Interactive proof exploration

5. **Language Extensions**
   - DCG support
   - Constraint handling
   - Tabling directives
