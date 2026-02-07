# Contributing to MI

Thank you for your interest in contributing to the Mathematical Induction Converter for Prolog!

## Getting Started

1. **Fork the repository**
   ```bash
   git clone https://github.com/luciangreen/mi.git
   cd mi
   ```

2. **Ensure you have SWI-Prolog installed**
   ```bash
   swipl --version  # Should be 7.0+
   ```

3. **Run the tests**
   ```prolog
   ?- [tests/test_mi].
   ?- run_all_tests.
   ```

## Development Workflow

1. **Create a feature branch**
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. **Make your changes**
   - Follow the coding style (see below)
   - Add tests for new functionality
   - Update documentation as needed

3. **Test your changes**
   ```prolog
   ?- [tests/test_mi].
   ?- run_all_tests.
   ```

4. **Run the demo**
   ```prolog
   ?- [demo].
   ?- run_all_demos.
   ```

5. **Commit with clear messages**
   ```bash
   git commit -m "Add feature: description"
   ```

6. **Submit a pull request**

## Coding Style

### Prolog Conventions

1. **Predicate Names**
   - Use snake_case: `detect_recursive/2`
   - Be descriptive: `build_equation_for_recursive/4`

2. **Variable Names**
   - CamelCase for variables: `RecInfo`, `ClosedForms`
   - Descriptive names: `Equations` not `E`
   - Underscore prefix for unused: `_Unused`

3. **Comments**
   ```prolog
   %% Section header (double percent)
   % Single line comment (single percent)
   
   %% Main predicate documentation
   my_predicate(Input, Output) :-
       % Step-by-step comment
       do_something(Input, Intermediate),
       do_more(Intermediate, Output).
   ```

4. **Module Structure**
   ```prolog
   :- module(name, [
       exported_pred/2,
       another_pred/3
   ]).
   
   %% Exported predicates first
   
   %% Helper predicates last
   ```

5. **Formatting**
   - Indent with 4 spaces
   - Align clause heads
   - One blank line between predicates
   - Two blank lines between sections

### Documentation

1. **Every exported predicate needs documentation**
   ```prolog
   %% predicate_name(+Input, -Output)
   %  Description of what it does.
   %  
   %  @param Input  Description of input
   %  @param Output Description of output
   ```

2. **Update README.md** for user-facing changes

3. **Update USAGE.md** for API changes

4. **Update ARCHITECTURE.md** for design changes

## Testing Guidelines

### Writing Tests

1. **Use descriptive test names**
   ```prolog
   test(detect_factorial_recursive) :- ...
   ```

2. **Test edge cases**
   - Empty lists
   - Single elements
   - Non-recursive predicates
   - Complex nested structures

3. **Test error conditions**
   ```prolog
   test(invalid_input_fails) :-
       \+ detect_recursive(invalid, _).
   ```

4. **Group related tests**
   ```prolog
   :- begin_tests(feature_name).
   % tests here
   :- end_tests(feature_name).
   ```

### Test Coverage

Aim for:
- 100% of exported predicates
- All recursion types
- Edge cases
- Integration tests

## Areas for Contribution

### High Priority

1. **Enhanced Gaussian Elimination**
   - Full symbolic row operations
   - Better equation solving
   - Matrix manipulation

2. **More Recursion Patterns**
   - Mutual recursion
   - Indirect recursion
   - Multi-predicate analysis

3. **Better Closed-Form Derivation**
   - Non-linear recurrences
   - More pattern types
   - Symbolic math improvements

### Medium Priority

1. **Performance Optimization**
   - Faster pattern mining
   - Memoization
   - Parallel analysis

2. **Better Output**
   - Formatted Prolog
   - Visual proofs
   - Interactive mode

3. **More Examples**
   - Real-world predicates
   - Edge cases
   - Performance comparisons

### Nice to Have

1. **Tool Integration**
   - Editor plugins
   - CI/CD integration
   - Web interface

2. **Documentation**
   - Video tutorials
   - More examples
   - Theory background

3. **Language Support**
   - DCG support
   - Constraints
   - Tabling

## Code Review Process

Pull requests will be reviewed for:

1. **Correctness**
   - Does it work as intended?
   - Are there edge cases?
   - Any bugs introduced?

2. **Tests**
   - Are new features tested?
   - Do all tests pass?
   - Is coverage maintained?

3. **Documentation**
   - Are changes documented?
   - Are examples updated?
   - Is API clear?

4. **Style**
   - Follows coding conventions?
   - Readable and maintainable?
   - Well-commented?

5. **Design**
   - Fits architecture?
   - Minimal changes?
   - No breaking changes?

## Questions?

- Open an issue for bugs or feature requests
- Start a discussion for design questions
- Check existing issues before creating new ones

## License

By contributing, you agree that your contributions will be licensed under the same license as the project (see LICENSE file).

## Recognition

Contributors will be acknowledged in:
- README.md (Contributors section)
- Release notes
- Git commit history

Thank you for contributing! 🙏
