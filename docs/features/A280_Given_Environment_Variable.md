<!-- omit from toc -->
## Feature: environment variables

A `Given` step sets or unsets an environment variable for the commands run
afterwards in the scenario. Each variable gets its previous value (or
absence) back at the end of the scenario.

_Table of Contents:_
- [Scenario: set a variable](#scenario-set-a-variable)
- [Scenario: the variable is back to its previous state](#scenario-the-variable-is-back-to-its-previous-state)
- [Scenario: unset a variable](#scenario-unset-a-variable)
- [Scenario: the unset variable is back](#scenario-the-unset-variable-is-back)
- [Scenario: set twice in the same scenario](#scenario-set-twice-in-the-same-scenario)
- [Scenario: variable set in a background](#scenario-variable-set-in-a-background)

### Scenario: set a variable

- Given the environment variable `BBT_TEST_VARIABLE` is `hello`
- When I run `./sut read_env BBT_TEST_VARIABLE`
- Then the output is `hello`

### Scenario: the variable is back to its previous state

`BBT_TEST_VARIABLE` was not set before the previous scenario.

- When I run `./sut read_env BBT_TEST_VARIABLE`
- Then I get an error
- And the output contains `No BBT_TEST_VARIABLE environment variable`

### Scenario: unset a variable

- Given the environment variable `HOME` is not set
- When I run `./sut read_env HOME`
- Then I get an error
- And the output contains `No HOME environment variable`

### Scenario: the unset variable is back

- When I run `./sut read_env HOME`
- Then I get no error

### Scenario: set twice in the same scenario

- Given the environment variable `BBT_TEST_VARIABLE` is `first`
- And the environment variable `BBT_TEST_VARIABLE` is `second`
- When I run `./sut read_env BBT_TEST_VARIABLE`
- Then the output is `second`

### Scenario: variable set in a background

- Given the new `env_background.md` file
```md
## Background:
- Given the environment variable `BBT_TEST_VARIABLE` is `from background`

## Scenario: first
- When I run `./sut read_env BBT_TEST_VARIABLE`
- Then the output is `from background`

## Scenario: second
- When I run `./sut read_env BBT_TEST_VARIABLE`
- Then the output is `from background`
```

- When I run `./bbt env_background.md`
- Then I get no error
