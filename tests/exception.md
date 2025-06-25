<!-- omit from toc -->
## Feature: Deprecated and no more supported options

bbt warn the user that an option may be removed in the future, and suggest a solution.

### Scenario: --output option
- Given the new `ver.md` file
  ~~~
  # Scenario
  - When I successfully run `./sut -v`
  ~~~
- Given there is no `tmp.md` file