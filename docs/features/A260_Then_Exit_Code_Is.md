<!-- omit from toc -->
## Feature: exact exit code

`Then I get an error` / `Then I get no error` only tell success from failure.
When the exit code itself is part of the specification (a usage error is 2,
a missing file is 66…), the step `Then the exit code is `n`` checks it.

_Table of Contents:_
- [Scenario: exit code of a successful command](#scenario-exit-code-of-a-successful-command)
- [Scenario: exit code of a failing command](#scenario-exit-code-of-a-failing-command)
- [Scenario: the exit code of the last command is checked](#scenario-the-exit-code-of-the-last-command-is-checked)
- [Scenario: wrong exit code](#scenario-wrong-exit-code)
- [Scenario: the code must be an integer](#scenario-the-code-must-be-an-integer)

### Scenario: exit code of a successful command

- When I run `./sut -v`
- Then the exit code is `0`
- And I get no error

### Scenario: exit code of a failing command

`sut delay n code` returns `code` after n seconds.

- When I run `./sut delay 0 3`
- Then the exit code is `3`
- And I get an error

### Scenario: the exit code of the last command is checked

- When I run `./sut delay 0 4`
- And I run `./sut -v`
- Then the exit code is `0`

### Scenario: wrong exit code

- Given the new `wrong_exit_code.md` file
```md
# Scenario: Wrong exit code
- When I run `./sut delay 0 3`
- Then the exit code is `2`
```

- When I run `./bbt wrong_exit_code.md`
- Then I get an error
- And the output contains `Expected exit code 2, got 3`

### Scenario: the code must be an integer

- Given the new `not_a_code.md` file
```md
# Scenario: Not a code
- When I run `./sut -v`
- Then the exit code is `zero`
```

- When I run `./bbt not_a_code.md`
- Then I get an error
- And the output contains `Exit code expected in object phrase, got "zero"`
