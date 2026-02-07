# Example Files

This directory contains example Prolog files for testing the MI (Mathematical Induction) converter.

## Files

- `factorial.pl` - Contains examples of different recursion patterns:
  - `factorial/2` - Simple recursive function
  - `fibonacci/2` - Nested recursive (multiple recursive calls)
  - `list_length/2` - Tail recursive
  - `sum_list/3` - Recursive with accumulator

## Usage

To process an example file:

```prolog
?- use_module(mi).
?- process_file('examples/factorial.pl', 'examples/factorial_output.pl').
```

This will:
1. Analyze the recursive predicates
2. Build equation systems
3. Flatten the recursion
4. Derive closed-form solutions
5. Mine pattern reuse opportunities
6. Generate optimized Prolog code
7. Create proof notes and test cases
