<!-- omit from toc -->
## Feature : warning on shell metacharacters in commands

bbt does not run commands through a shell: pipes (`|`), command substitutions (`$(...)`, backticks), redirections (`>`, `<`), and command sequencing (`&&`, `;`) are passed verbatim as arguments to the command. As a step written with such metacharacters can never behave as its author expects, bbt raises a warning when a run command contains shell metacharacters.  
Quoting is on the other hand the way to pass a literal metacharacter as an argument when commands are not run through a shell, so no warning is raised for metacharacters inside a quoted part of the command.  
When the metacharacters must be interpreted, the command can be run through an explicit shell, e.g. `sh -c 'cmd1 | cmd2'`.

_Table of Contents:_
- [Scenario: warning when a command contains a pipe](#scenario-warning-when-a-command-contains-a-pipe)
- [Scenario: warning also displayed by bbt explain](#scenario-warning-also-displayed-by-bbt-explain)
- [Scenario: no warning when the metacharacter is quoted (Unix\_Only)](#scenario-no-warning-when-the-metacharacter-is-quoted-unix_only)
- [Scenario: no warning on a quoted metacharacter in bbt explain](#scenario-no-warning-on-a-quoted-metacharacter-in-bbt-explain)
- [Scenario: no warning on an escaped quote inside a quoted command (Unix\_Only)](#scenario-no-warning-on-an-escaped-quote-inside-a-quoted-command-unix_only)

## Scenario: warning when a command contains a pipe

- Given the new file `pipe_test.md`
  ~~~
  # Scenario
  - When I run `./sut --version | grep sut`
  ~~~

- When I run `./bbt -c pipe_test.md`
- Then output is
  ```
  pipe_test.md:2: Warning: the command contains a shell metacharacter ('|'), but commands are not run through a shell: quote it to pass it as a literal argument, or run a shell, e.g. `sh -c 'cmd1 | cmd2'`

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
  pipe_test.md:2: Warning: the command contains a shell metacharacter ('|'), but commands are not run through a shell: quote it to pass it as a literal argument, or run a shell, e.g. `sh -c 'cmd1 | cmd2'`

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


## Scenario: no warning on an escaped quote inside a quoted command (Unix_Only)

This is the pattern found in the smk test suite: a shell command that
selects a field with `cut -d'"'`, written in a scenario as `cut -d'\"'`,
followed by a redirection. All the metacharacters are inside the double
quoted argument of the `sh -c`, and must not raise a warning.

- Given the new file `escaped_quote_test.md`
  ~~~
  # Scenario
  - Given the new file `alire.toml`
    ```
    version = "1.2.3"
    ```
  - When I run `sh -c "grep ^version alire.toml | cut -d'\"' -f2 > out.version.txt"` Successfully
  ~~~

- When I run `./bbt -c escaped_quote_test.md`
- Then output is
  ```
  # Document: [escaped_quote_test.md](escaped_quote_test.md)
  ### Scenario: [](escaped_quote_test.md):
  - OK : Given the new file `alire.toml`
  - OK : When I run `sh -c "grep ^version alire.toml | cut -d'\"' -f2 > out.version.txt"` Successfully
  - [X] scenario [](escaped_quote_test.md) pass

  ## Summary : **Success**, 1 scenarios OK
  ```

## Scenario: no warning on a quoted metacharacter in bbt explain

- Given the new file `quoted_glob_test.md`
  ~~~
  # Scenario
  - When I run `find . -name "*.ad[sb]"` Successfully
  ~~~

- When I run `./bbt explain quoted_glob_test.md`
- Then output is
  ```
  Document `quoted_glob_test.md`

  1: Scenario ``
  2: - Run command `find . -name "*.ad[sb]"`
  ```
