# Changelog

All notable changes to the MI (Mathematical Induction Converter) project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2026-02-05

### Added

#### Core Functionality
- **Prolog Clause Parser** - Parse and structure Prolog clauses from files and terms
- **Recursion Detection** - Detect and classify recursive predicates:
  - Tail recursive detection
  - Nested recursive detection (multiple recursive calls)
  - General recursive classification
- **Subterm-Address Indexing** - Precise term location tracking for equation building
- **Equation System Builder** - Convert recursive predicates to equation systems
- **Recursion Flattening** - Replace recursive calls with symbolic variables (R1, R2, ...)
- **Gaussian Elimination** - Symbolic equation solving (simplified implementation)
- **Closed-Form Derivation** - Derive mathematical formulas from recursive definitions
- **Pattern Mining** - Discover repeated subterm patterns across predicates
- **Single-Use Tagging** - Identify and tag single-use vs. reusable patterns
- **Code Generation** - Emit optimized Prolog with helper predicates

#### Output Generation
- Rewritten Prolog code with extracted helper functions
- Proof notes with pattern analysis
- Optimization decisions documentation
- Generated test cases for validation

#### API Predicates
- `process_file/2` - Main entry point for file processing
- `analyze_predicates/2` - Programmatic analysis without file I/O
- `parse_clauses/2` - Parse Prolog terms
- `detect_recursive/2` - Detect recursive predicates
- `build_equations/2` - Build equation systems
- `flatten_recursion/2` - Flatten with symbolic variables
- `infer_relationships/2` - Apply Gaussian elimination
- `derive_closed_form/2` - Derive closed-form solutions
- `mine_patterns/2` - Mine repeated patterns
- `tag_single_use/2` - Tag single-use elements
- `emit_rewritten/3` - Generate output files
- `term_depth/2` - Utility for term depth calculation

#### Documentation
- **README.md** - Comprehensive project overview
- **USAGE.md** - Detailed usage guide with examples
- **ARCHITECTURE.md** - System design and implementation details
- **CONTRIBUTING.md** - Contribution guidelines
- **examples/README.md** - Example files documentation

#### Examples
- `examples/factorial.pl` - Factorial, Fibonacci, list operations
- `examples/simple_example.pl` - Simple pattern demonstration

#### Testing
- Comprehensive test suite with 19 tests covering:
  - Clause parsing
  - Recursion detection (all types)
  - Equation building
  - Recursion flattening
  - Pattern mining
  - Single-use tagging
  - Integration tests
  - Utility functions
- All tests passing

#### Demonstrations
- `demo.pl` - Interactive demonstration script:
  - Basic clause analysis
  - Full pipeline demonstration
  - Pattern mining showcase
  - File processing example
- `run_all_demos` - Complete demo runner
- `quick_test` - Quick validation

### Technical Details

#### Algorithm Implementations
- Subterm address extraction with path-based indexing
- Recursive call identification via term traversal
- Symbolic variable generation and substitution
- Pattern frequency analysis and deduplication
- Helper predicate name generation via hashing

#### Quality Improvements
- Zero Prolog warnings in main module
- All singleton variables properly handled
- Module exports properly defined
- Comprehensive error handling

### Known Limitations

- Gaussian elimination is simplified for symbolic terms (full row operations planned)
- Closed-form solutions work best with linear recurrences
- Complex nested patterns may require manual inspection
- File I/O requires valid Prolog syntax

### Dependencies

- SWI-Prolog 7.0 or higher

### Repository Structure

```
mi/
├── mi.pl                    # Main module
├── README.md                # Project overview
├── USAGE.md                 # Usage guide
├── ARCHITECTURE.md          # Design documentation
├── CONTRIBUTING.md          # Contribution guidelines
├── CHANGELOG.md             # This file
├── LICENSE                  # License file
├── demo.pl                  # Demonstration script
├── examples/
│   ├── README.md            # Examples documentation
│   ├── factorial.pl         # Example recursive predicates
│   └── simple_example.pl    # Simple patterns
└── tests/
    └── test_mi.pl           # Test suite
```

### Contributors

- Lucian Green - Initial implementation
- GitHub Copilot - Development assistance

---

## [Unreleased]

### Planned Features

#### High Priority
- Full Gaussian elimination with symbolic row operations
- Support for mutual recursion
- Non-linear recurrence solving
- Enhanced pattern detection

#### Medium Priority
- Performance optimizations (memoization, parallelization)
- Better formatted output with comments
- Visual proof generation
- More comprehensive examples

#### Future Enhancements
- DCG support
- Constraint handling
- Tabling directives
- Editor integration
- Web interface

---

[1.0.0]: https://github.com/luciangreen/mi/releases/tag/v1.0.0
[Unreleased]: https://github.com/luciangreen/mi/compare/v1.0.0...HEAD
