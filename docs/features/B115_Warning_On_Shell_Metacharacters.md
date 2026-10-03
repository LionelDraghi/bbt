<!-- omit from toc -->
## Feature : warning on shell metacharacters in commands

bbt does not run commands through a shell: pipes (`|`), command substitutions (`$(...)`, backticks), redirections (`>`, `<`), and command sequencing (`&&`, `;`) are passed verbatim as arguments to the command. As a step written with such metacharacters can never behave as its author expects, bbt raises a warning when a run command contains shell metacharacters.  
Quoting is on the other hand the way to pass a literal metacharacter as an argument when commands are not run through a shell, so no warning is raised for metacharacters inside a quoted part of the command.

_Table of Contents:_
- [Scenario: warning when a command contains a pipe](#scenario-warning-when-a-command-contains-a-pipe)
- [Scenario: warning also displayed by bbt explain](#scenario-warning-also-displayed-by-bbt-explain)
- [Scenario: no warning when the metacharacter is quoted (Unix\_Only)](#scenario-no-warning-when-the-metacharacter-is-quoted-unix_only)
- [Scenario: no warning on a quoted metacharacter in bbt explain](#scenario-no-warning-on-a-quoted-metacharacter-in-bbt-explain)

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

## Scenario: no warning when the metacharacter is quoted (Unix_Only)

- Given the new file `quoted_glob_test.md`
  ~~~
  # Scenario
  - When I run `find . -name "*.ad[sb]"` Successfully
  ~~~

- When I run `./bbt -c quoted_glob_test.md`
- Then output is
  ```
  # Document: [quoted_glob_test.md](quoted_glob_test.md)
  ### Scenario: [](quoted_glob_test.md):
  - OK : When I run `find . -name "*.ad[sb]"` Successfully
  - [X] scenario [](quoted_glob_test.md) pass

  ## Summary : **Success**, 1 scenarios OK
  ```

## Scenario: no warning on a quoted metacharacter in bbt explain

- When I run `./bbt explain quoted_glob_test.md`
- Then output is
  ```
  Document `quoted_glob_test.md`

  1: Scenario ``
  2: - Run command `find . -name "*.ad[sb]"`
  ```
