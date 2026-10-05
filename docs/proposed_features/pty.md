<!-- omit from toc -->
## Feature: pseudo terminal for interactive commands

A command started by a scenario containing `type` or `enter` steps is fed
through pipes. A program that does not flush its prompt before waiting for
input will never show it: the pipes are not terminals, and the standard
output is then block buffered. `Ada.Text_IO` programs are not concerned,
the output being flushed before any read, but a C program using stdio
needs an explicit `fflush`, and that is a constraint on the software
under test.

The proposal: such a command gets a pseudo terminal for its standard
input and standard output (the Spawn library `Set_Standard_Input_PTY`
and `Set_Standard_Output_PTY`), so that the program behaves as if run
interactively in a terminal: the standard output is line buffered, the
prompts are visible before the program waits for input, and there is
no more constraint on the software under test.

Two behaviors of the terminal driver must be taken care of before the
output is checked, so that the expected outputs stay identical with or
without pseudo terminal:

- the line feeds written by the program are translated into `CR` `LF`
  on their way out of the terminal: bbt filters the `CR`, so that
  `Then the output is `msg`` matches the same text as over pipes;
- the terminal echoes the sent input to the output: the echo must not
  be part of the checked output, either by disabling the echo on the
  terminal, or by filtering it.

This applies on POSIX systems only: on Windows, the commands keep the
pipes behavior, and the flush constraint remains.

## Experiment results (2026-10-05)

The Spawn library (25.0.0) provides the pseudo terminal
(`posix_openpt`, `grantpt`, `unlockpt`), but leaves it in the terminal
default mode: no `tcsetattr` is done, and no file descriptor of the
pseudo terminal is exposed by the API, so bbt cannot configure it.

With such an unconfigured terminal (observed with `script`, which
allocates one):

- the prompts are visible before the program waits for input: the
  motivating gain is real;
- the sent input is echoed back into the output: the echo would be
  part of the checked output, unless bbt filters the exact bytes it
  has just sent, a heuristic that may fail on a program legitimately
  starting its output with the same text;
- the line feeds are translated into `CR` `LF`: bbt must filter the
  `CR` before the checks;
- a single key without `Enter` is not delivered to a C program
  reading with `getchar` (canonical mode), while it is delivered to an
  `Ada.Text_IO` program, `Get_Immediate` temporarily switching the
  terminal mode.

Two possible paths, to be arbitrated:

1. bbt filters the `CR` and the echo itself, and accepts the
   single-key limitation for non Ada programs: no new dependency, but
   a filtering heuristic in the output path, and a behavior that
   diverges between POSIX and Windows;
2. an upstream evolution of the Spawn library, exposing a pseudo
   terminal configuration (no echo, no canonical mode, no `CR` `LF`
   translation): the clean path, making the checked output identical
   with or without pseudo terminal.


_Table of Contents:_
- [Scenario: answering a C program that does not flush](#scenario-answering-a-c-program-that-does-not-flush)
- [Scenario: the line feeds are checked without the carriage returns](#scenario-the-line-feeds-are-checked-without-the-carriage-returns)
- [Scenario: the input echo is ignored](#scenario-the-input-echo-is-ignored)

### Scenario: answering a C program that does not flush

This is the motivating case: the program below has no `fflush`, and
would fail today.

- Given the new file `ask.c` containing
  ```c
  #include <stdio.h>
  int main() {
    char c;
    printf("Continue? [y/n]\n");
    c = getchar();
    if (c == 'y') {
      printf("Continuing\n");
    }
    return 0;
  }
  ```
- When I successfully run `gcc ask.c -o ask`
- When I run `./ask`
- Then the output is `Continue? [y/n]`
- When I type `y`
- Then the output is `Continuing`

### Scenario: the line feeds are checked without the carriage returns

The terminal translates the line feeds of the program into `CR` `LF`.
The expected outputs are the same as in
[A290_When_I_Type_Or_Enter](../features/A290_When_I_Type_Or_Enter.md),
they must not depend on the terminal translation.

- Given the new file `to_delete.txt` containing `some data`
- When I run `./sut delete to_delete.txt`
- Then the output is
```
Delete to_delete.txt? [Y]es/[N]o
```
- When I type `Y`
- Then the output is `Deleting to_delete.txt`
- And there is no file `to_delete.txt`

### Scenario: the input echo is ignored

With a terminal, the `Y` sent to the program would normally be echoed
to the output. The echo must not be part of the checked output: the
consequence of the input is `Deleting`, not `Y Deleting`.

- Given the new file `to_delete.txt` containing `some data`
- When I run `./sut delete to_delete.txt`
- When I type `Y`
- Then the output is `Deleting to_delete.txt`
