<!-- omit from toc -->
## Feature: Deprecated and no more supported options

bbt should warn the user if an option should be removed in the future, and suggest a solution.

### Scenario: --output option
- Given the new `ver.md` file
  ~~~
  # Scenario
  - When I successfully run `./sut -v`
  ~~~

- When I run `./bbt -c --yes ver.md --output tmp.md` 

- Then the output contains 
  ~~~
  Warning: -o and --output options deprecated, use --index instead
  ~~~

### Scenario: -sb and --status_bar options

The status bar is now the default, and the option is no longer
necessary: bbt warns and runs as if it was not there.

- Given the new `ver.md` file
  ~~~
  # Scenario
  - When I successfully run `./sut -v`
  ~~~

- When I run `./bbt -c --yes ver.md -sb`

- Then the output contains 
  ~~~
  Warning: -sb and --status_bar options deprecated, the status bar is now enabled by default, use --no_tty to disable it
  ~~~


