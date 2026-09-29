<!-- omit from toc -->
## Feature: error output

By default, the output checked by `output` steps is the standard output and
the standard error of the command, merged.

When a scenario has an `error output` step, the commands it runs write
their standard error apart: `error output` steps check the standard error,
and `output` steps check the standard output alone.

_Table of Contents:_
- [Scenario: error message on the error output](#scenario-error-message-on-the-error-output)
- [Scenario: nothing on the error output](#scenario-nothing-on-the-error-output)
- [Scenario: error output contains and does not contain](#scenario-error-output-contains-and-does-not-contain)
- [Scenario: error output in a code block](#scenario-error-output-in-a-code-block)
- [Scenario: without error output step, the outputs stay merged](#scenario-without-error-output-step-the-outputs-stay-merged)
- [Scenario: wrong error output](#scenario-wrong-error-output)

### Scenario: error message on the error output

- When I run `./sut create`
- Then the error output is `Missing file name`
- And there is no output

### Scenario: nothing on the error output

- When I run `./sut -v`
- Then there is no error output
- And the output is `sut version 1.0`

### Scenario: error output contains and does not contain

- When I run `./sut read_env BBT_SURELY_UNSET_VARIABLE`
- Then the error output contains `BBT_SURELY_UNSET_VARIABLE`
- And the error output does not contain `version`
- When I run `./sut -v`
- Then I get no error output

### Scenario: error output in a code block

- When I run `./sut read_env BBT_SURELY_UNSET_VARIABLE`
- Then the error output is
```
No BBT_SURELY_UNSET_VARIABLE environment variable
```

### Scenario: without error output step, the outputs stay merged

- When I run `./sut create`
- Then the output is `Missing file name`

### Scenario: wrong error output

- Given the new `wrong_error_output.md` file
```md
# Scenario: Wrong error output
- When I run `./sut -v`
- Then the error output contains `version`
```

- When I run `./bbt wrong_error_output.md`
- Then I get an error
