<!-- omit from toc -->
## Feature : warning on shell metacharacters in commands

bbt does not run commands through a shell: pipes (`|`), command substitutions (`$(...)`, backticks), redirections (`>`, `<`), and command sequencing (`&&`, `;`) are passed verbatim as arguments to the command. As a step written with such metacharacters can never behave as its author expects, bbt raises a warning when a run command contains shell metacharacters.

_Table of Contents:_
- [Scenario: warning when a command contains a pipe](#scenario-warning-when-a-command-contains-a-pipe)
- [Scenario: warning also displayed by bbt explain](#scenario-warning-also-displayed-by-bbt-explain)

## Scenario: warning when a command contains a pipe

- Given the new file `pipe_test.md`
  ~~~
  # Scenario
  - When I run `./sut --version | grep sut`
  ~~~

- When I run `./bbt -c pipe_test.md`
- Then output is
  ```
  pipe_test.md:2: Warning: the command contains a shell metacharacter ('|'), but commands are not run through a shell: a command shall not contain pipes, redirections, or command substitutions; '|' will be passed as an argument to the command

  # Document: [pipe_test.md](pipe_test.md)
  ### Scenario: [](pipe_test.md):
  - OK : When I run `./sut --version | grep sut`
  - [X] scenario [](pipe_test.md) pass

  ## Summary : **Success**, 1 scenarios OK
  ```

## Scenario: warning also displayed by bbt explain

- When I run `./bbt explain pipe_test.md`
- Then output is
  ```
  pipe_test.md:2: Warning: the command contains a shell metacharacter ('|'), but commands are not run through a shell: a command shall not contain pipes, redirections, or command substitutions; '|' will be passed as an argument to the command

  Document `pipe_test.md`

  1: Scenario ``
  2: - Run command `./sut --version | grep sut`
  ```
