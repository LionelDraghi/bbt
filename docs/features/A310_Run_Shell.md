<!-- omit from toc -->
## Feature : run shell

`When I run shell `cmd`` runs the command string through a shell: the
whole string is passed as a single argument to the shell, without being
split by bbt, so that pipes, redirections and command substitutions are
interpreted by that shell, contrary to the plain run step that spawns
the command directly (cf. [B110_Spawn.md](B110_Spawn.md) and
[B115_Warning_On_Shell_Metacharacters.md](B115_Warning_On_Shell_Metacharacters.md)).
No shell metacharacter warning is raised on a shell step: the
metacharacters are then expected to be interpreted by the shell.

The `successfully run shell` variant checks in addition that the shell
command does not return an error.

The shell is set with the `--shell` option or the `BBT_SHELL`
environment variable, among `sh`, `bash`, `dash`, `zsh`, `fish`, `cmd`,
`powershell` and `pwsh`. It defaults to `sh` on POSIX systems and to
`cmd.exe` on Windows, whose redirections have the same semantics as the
POSIX ones (cf. [D11](../dev/design_discussions.md)).

_Table of Contents:_
- [Scenario: successfully run shell](#scenario-successfully-run-shell)
- [Scenario: shell redirections are interpreted by the shell](#scenario-shell-redirections-are-interpreted-by-the-shell)
- [Scenario: shell pipes are interpreted by the shell (Unix\_Only)](#scenario-shell-pipes-are-interpreted-by-the-shell-unix_only)
- [Scenario: the exit code is the shell command one (Unix\_Only)](#scenario-the-exit-code-is-the-shell-command-one-unix_only)
- [Scenario: shell redirections on Windows (Windows\_Only)](#scenario-shell-redirections-on-windows-windows_only)
- [Scenario: no warning on shell metacharacters in a shell step](#scenario-no-warning-on-shell-metacharacters-in-a-shell-step)
- [Scenario: invalid shell on the command line](#scenario-invalid-shell-on-the-command-line)
- [Scenario: the shell is set on the command line (Unix\_Only)](#scenario-the-shell-is-set-on-the-command-line-unix_only)
- [Scenario: the shell is set with the BBT\_SHELL environment variable (Unix\_Only)](#scenario-the-shell-is-set-with-the-bbt_shell-environment-variable-unix_only)
- [Scenario: the sh shell is set on the command line (Unix\_Only)](#scenario-the-sh-shell-is-set-on-the-command-line-unix_only)
- [Scenario: the dash shell is set on the command line (Unix\_Only) (needs\_dash)](#scenario-the-dash-shell-is-set-on-the-command-line-unix_only-needs_dash)
- [Scenario: the zsh shell is set on the command line (Unix\_Only) (needs\_zsh)](#scenario-the-zsh-shell-is-set-on-the-command-line-unix_only-needs_zsh)
- [Scenario: the fish shell is set on the command line (Unix\_Only) (needs\_fish)](#scenario-the-fish-shell-is-set-on-the-command-line-unix_only-needs_fish)
- [Scenario: the cmd shell is set on the command line (Windows\_Only)](#scenario-the-cmd-shell-is-set-on-the-command-line-windows_only)
- [Scenario: the powershell shell is set on the command line (Windows\_Only)](#scenario-the-powershell-shell-is-set-on-the-command-line-windows_only)
- [Scenario: the pwsh shell is set on the command line (needs\_pwsh)](#scenario-the-pwsh-shell-is-set-on-the-command-line-needs_pwsh)

## Scenario: successfully run shell

- When I successfully run shell `echo Hello`
- Then I get no error
- And the output contains `Hello`

## Scenario: shell redirections are interpreted by the shell

- Given there is no `hello.txt` file
- When I successfully run shell `echo Hello > hello.txt`
- Then the file `hello.txt` contains `Hello`

## Scenario: shell pipes are interpreted by the shell (Unix_Only)

- When I run shell `./sut -v | grep sut`
- Then I get no error
- And the output contains `sut`

## Scenario: the exit code is the shell command one (Unix_Only)

- When I run shell `./sut delay 1 2`
- Then the exit code is `2`

## Scenario: shell redirections on Windows (Windows_Only)

On Windows the default shell is `cmd.exe`, that resolves a bare command
name in the current directory, and whose `>` redirection writes raw
bytes, as `sh` does.

- Given there is no `out.version.txt` file
- When I successfully run shell `sut -v > out.version.txt`
- Then the file `out.version.txt` contains `sut`

## Scenario: no warning on shell metacharacters in a shell step

- Given the new file `shell_test.md`
  ~~~
  # Scenario
  - When I run shell `./sut -v | grep sut`
  ~~~

- When I run `./bbt explain shell_test.md`
- Then I get no error
- And the output does not contain `Warning`
- And the output contains `in a shell`

## Scenario: invalid shell on the command line

- Given the new file `shell_test.md`
  ~~~
  # Scenario
  - When I run shell `./sut -v`
  ~~~

- When I run `./bbt --shell=xyzabc -c shell_test.md`
- Then I get an error
- And the output contains `invalid shell "xyzabc"`

## Scenario: the shell is set on the command line (Unix_Only)

`$BASH_VERSION` is set only by bash: the nested scenario fails if the
command is run by the default shell instead of the `--shell` one.

- Given the new file `shell_test.md`
  ~~~
  # Scenario
  - When I run shell `echo $BASH_VERSION`
  - Then the output contains `release`
  ~~~

- When I run `./bbt --shell=bash -c shell_test.md`
- Then I get no error
- And the output contains `## Summary : **Success**, 1 scenarios OK`

## Scenario: the shell is set with the BBT_SHELL environment variable (Unix_Only)

With the same nested file, the run without `BBT_SHELL` set fails on
the default shell, and succeeds once the variable selects bash: this
is the proof that the environment variable is taken into account.

- Given the new file `shell_test.md`
  ~~~
  # Scenario
  - When I run shell `echo $BASH_VERSION`
  - Then the output contains `release`
  ~~~

- When I run `./bbt -c shell_test.md`
- Then I get an error
- And the output contains `**Fail**`
- Given the environment variable `BBT_SHELL` is `bash`
- When I run `./bbt -c shell_test.md`
- Then I get no error
- And the output contains `## Summary : **Success**, 1 scenarios OK`

The following scenarios check the whole chain: that the shell actually
launched is the configured one. `${0##*/}` gives the name of the shell
that runs the command string, so a nested scenario asserting that name
fails if another shell runs the command string. The scenarios needing a
shell that is not preinstalled on every platform are tagged
`needs_<shell>` and excluded by the test harness when that shell is not
in the PATH (cf. tests/Makefile).

## Scenario: the sh shell is set on the command line (Unix_Only)

- Given the new file `shell_test.md`
  ~~~
  # Scenario
  - When I successfully run shell `echo ${0##*/}`
  - Then the output is `sh`
  ~~~

- When I run `./bbt --shell=sh -c shell_test.md`
- Then I get no error
- And the output contains `## Summary : **Success**, 1 scenarios OK`

## Scenario: the dash shell is set on the command line (Unix_Only) (needs_dash)

- Given the new file `shell_test.md`
  ~~~
  # Scenario
  - When I successfully run shell `echo ${0##*/}`
  - Then the output is `dash`
  ~~~

- When I run `./bbt --shell=dash -c shell_test.md`
- Then I get no error
- And the output contains `## Summary : **Success**, 1 scenarios OK`

## Scenario: the zsh shell is set on the command line (Unix_Only) (needs_zsh)

- Given the new file `shell_test.md`
  ~~~
  # Scenario
  - When I successfully run shell `echo ${0##*/}`
  - Then the output is `zsh`
  ~~~

- When I run `./bbt --shell=zsh -c shell_test.md`
- Then I get no error
- And the output contains `## Summary : **Success**, 1 scenarios OK`

## Scenario: the fish shell is set on the command line (Unix_Only) (needs_fish)

fish has no `$0`: its `$version` variable identifies it.

- Given the new file `shell_test.md`
  ~~~
  # Scenario
  - When I successfully run shell `echo $version`
  - Then the output matches `[0-9]+\.[0-9]+.*`
  ~~~

- When I run `./bbt --shell=fish -c shell_test.md`
- Then I get no error
- And the output contains `## Summary : **Success**, 1 scenarios OK`

## Scenario: the cmd shell is set on the command line (Windows_Only)

`%OS%` is expanded by cmd.exe only.

- Given the new file `shell_test.md`
  ~~~
  # Scenario
  - When I successfully run shell `echo %OS%`
  - Then the output contains `Windows_NT`
  ~~~

- When I run `./bbt --shell=cmd -c shell_test.md`
- Then I get no error
- And the output contains `## Summary : **Success**, 1 scenarios OK`

## Scenario: the powershell shell is set on the command line (Windows_Only)

`$PSVersionTable.PSVersion.Major` is `5` in Windows PowerShell, and is
not expanded by cmd.exe.

- Given the new file `shell_test.md`
  ~~~
  # Scenario
  - When I successfully run shell `echo $PSVersionTable.PSVersion.Major`
  - Then the output is `5`
  ~~~

- When I run `./bbt --shell=powershell -c shell_test.md`
- Then I get no error
- And the output contains `## Summary : **Success**, 1 scenarios OK`

## Scenario: the pwsh shell is set on the command line (needs_pwsh)

Same probe as the powershell scenario: PowerShell 7 answers `7`, so the
scenario fails if another shell runs the command string. pwsh is not
preinstalled on client Windows, the scenario is excluded when it is not
in the PATH.

- Given the new file `shell_test.md`
  ~~~
  # Scenario
  - When I successfully run shell `echo $PSVersionTable.PSVersion.Major`
  - Then the output is `7`
  ~~~

- When I run `./bbt --shell=pwsh -c shell_test.md`
- Then I get no error
- And the output contains `## Summary : **Success**, 1 scenarios OK`
