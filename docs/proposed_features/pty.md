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
  ``Then the output is `msg` `` matches the same text as over pipes;
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

## The ada-util path (2026-10-05)

[ada-util](https://github.com/stcarrez/ada-util) (Alire crate `utilada`,
by Stephane Carrez) turns out to provide exactly what is missing: its
`Util.Processes` package has a `Set_Allocate_TTY` procedure, and the
Unix implementation (`util-processes-os.adb`) configures the pseudo
terminal slave with `tcgetattr` / **`cfmakeraw`** / `tcsetattr`:

- raw mode means **no echo**: the input does not pollute the output;
- **no canonical mode**: a single key without `Enter` is delivered;
- **no `CR` `LF` translation**: the output bytes are exact.

So the three problems listed above are solved by construction, without
any upstream contribution. The rest of the API also fits bbt well:

- `Set_Environment (Proc, Name, Value)`: the environment steps map
  directly, with no snapshot trap (the environment is set on the child,
  not read from the parent);
- `Pipe_Mode` has a `READ_WRITE_ALL_SEPARATE` mode: three pipes, the
  standard error is received apart, and under TTY a second pseudo
  terminal is allocated for it, so the error output checks would keep
  working;
- `Set_Output_Stream (File)`, `Wait`, `Get_Exit_Status`, `Is_Running`,
  `Stop`.

On Windows, `Set_Allocate_TTY` is not implemented: the commands would
keep the pipes behavior, as with Spawn.

The weak point is the event model: reads on the pipe streams are
blocking, with no listener or polling equivalent to the Spawn
`Monitor_Loop`. The quiescence detection (`Wait_Quiet`, `Wait_Response`)
would then rely on a helper task, or on exposing the underlying file
descriptor.

## Experiment results (2026-10-09)

A prototype (tests/utilada_test, utilada 2.8.0) ran the motivating
scenario end to end on the pseudo terminal allocated by
`Util.Processes`: a C program with no `fflush`, prompting then reading
with `getchar`, answered by a single key without `Enter`.

All checks pass:

- the prompt is visible while the program waits on `getchar`: the pty
  makes the stdio standard output line buffered, the motivating gain
  is delivered as is;
- the single key without `Enter` is delivered: raw mode, no canonical
  mode;
- the key is not echoed, and no `CR` `LF` translation pollutes the
  output: `cfmakeraw` on the slave solves both by construction;
- the exit status is collected through `Wait` / `Get_Exit_Status`;
- `Set_Shell ("")` gives the bbt no-shell execution model, the
  argument splitting being then done by the library;
- `Set_Environment` maps the environment steps on the child.

The weak point is answered too, without any upstream contribution:
`Get_Output_Stream` returns a `Util.Streams.Raw.Raw_Stream`, whose
`Get_File` exposes the pty master descriptor. A `poll()` on that
descriptor implements the `Wait_Quiet` quiescence detection: read
while `POLLIN`, exit on the quiet timeout, on `POLLHUP` without
data, or on EOF. Two Ada gotchas on the way: bitwise `and` needs a
modular type, not a signed `Integer`, and `Util.Processes.Process`
is not tagged, so prefixed calls (`Proc.Spawn`) require `-gnatX`
whereas plain calls (`Spawn (Proc, ...)`) do not.

With this, candidate design 2 (replace Spawn by ada-util entirely)
has no identified unknown left; the remaining cost is the rewrite of
the command execution engine around blocking reads plus `poll`, and
the validation of the Windows paths. Candidate design 1 (contribute
the terminal configuration to Spawn upstream) keeps its dependency
advantage, but stays gated on the upstream acceptance and release
cycle.

## Implementation (2026-10-09)

Candidate design 2 is implemented on the `pty` branch: Spawn is
removed, and the command execution engine (`bbt-tests-actions-commands`)
is rewritten around `Util.Processes` plus a `poll()` on the output
and error descriptors exposed by `Raw_Stream.Get_File`. The commands
of a scenario containing `type` or `enter` steps run on a pseudo
terminal (`Set_Allocate_TTY`); the others keep the pipes behavior.

All the suites are green on Linux (features, examples, non_reg,
unit_testing), with two spec adaptations, both consequences of the
commands now being on a terminal:

- the flush constraint documented in A290 is gone: the feature text
  is updated accordingly;
- a program that detects the terminal changes behavior: the nested
  bbt of the A005 and B030 scenarios now runs with `--no_tty`, so
  that its status bar does not pollute its prompt.

The simulated terminal has a fixed size, 80x24 (the `BBT.Terminal`
package, called on the pty master after the command start): the
library allocates the pseudo terminal without a window size, and
an unset size (0x0) would make any program querying its terminal
size misbehave, while the point of the simulation is that the tested
program behaves as if a human had launched it in a terminal. The
size is set through `ioctl(TIOCSWINSZ)`, called through a thin C
wrapper (`src/posix/bbt_terminal.c`, the first C source of bbt):
`ioctl` is variadic, so Ada cannot import it directly, and the
request constant differs between Linux (16#5414#) and macOS
(16#80087468#), which only the C preprocessor resolves portably —
the same conclusion termicap reached for `TIOCGWINSZ`.

Not done yet, to be arbitrated with the Spawn / utilada comparison:
the validation of the Windows paths (the C `poll` import, and
`Set_Allocate_TTY` being not implemented on Windows, where the
commands keep the pipes behavior), and the move of the scenarios
below to docs/features once the design is arbitrated.

## Open questions (2026-10-09)

One subject raised while implementing, not arbitrated yet.

**The interactive scenarios cannot be fed through pipes anymore.**
With the pseudo terminal allocated to any scenario containing
`type` or `enter` steps, the "pre-scripted answers" context
(`printf 'y\n' | ask`) is not testable: a program detecting a
non-terminal standard input (batch mode, `sudo` refusing a piped
input) behaves differently there, and that behavior is a
legitimate test subject. Both deployment contexts are real lives
of a command line program — a component in a pipe chain, and an
interactive application — and a scenario should be able to test
either. The current rule (interactive steps imply the pseudo
terminal) infers the simulated deployment context from the steps;
the evolution is an explicit context in the scenario, for instance
``When I run in a terminal `./ask` ``, pipes staying the default:
the scenario would then state the deployment context it tests,
instead of the harness guessing it.

## Upstream proposals (2026-10-09)

Three functions that ada-util lacks would simplify the bbt engine,
and would add value to the library for anyone interacting with a
process. Filed on
[stcarrez/ada-util](https://github.com/stcarrez/ada-util):

1. `Wait_Event (Proc, Timeout) return Stream_Event` (`No_Event`,
   `Output_Available`, `Error_Available`, `Terminated`):
   [ada-util#72](https://github.com/stcarrez/ada-util/issues/72).
   Wait until output data or termination is ready. Implemented per
   platform by the library (`poll` on Unix,
   `WaitForMultipleObjects` on Windows), it would remove the bbt C
   `poll` binding and `Poll_Step` plumbing, and would make the bbt
   Windows port of the pump trivial. The most valuable of the three
   for both sides: the blocking reads are the known weak point of
   the library event model;

2. `Set_Terminal_Size (Proc, Rows, Cols)`:
   [ada-util#73](https://github.com/stcarrez/ada-util/issues/73),
   implemented by
   [PR ada-util#76](https://github.com/stcarrez/ada-util/pull/76)
   (October 2026, pending review).
   Stored in the `Process` record like `Set_Allocate_TTY`, and
   applied by the OS layer after the pseudo terminal opening. It
   would remove the bbt `BBT.Terminal` package and its C wrapper
   (the pty comes with a 0x0 window size, nonsense for any program
   querying its terminal size), and would spare every library user
   the variadic `ioctl(TIOCSWINSZ)` and its per platform request
   constant;

3. a non blocking termination check:
   [ada-util#74](https://github.com/stcarrez/ada-util/issues/74),
   implemented by
   [PR ada-util#75](https://github.com/stcarrez/ada-util/pull/75)
   (October 2026, pending review).
   `Wait (Proc, Timeout)` exposed and honored
   (the OS layer already takes a `Timeout` parameter, ignored on
   Unix and not visible in the public API), or a reaping
   `Is_Terminated`. `Is_Running` means today "not reaped yet", and is
   true for a terminated but not waited process; it would let bbt
   check the command liveness directly, instead of inferring the
   termination from the output streams end of file.

Candidate designs, to be arbitrated. The guiding design principle is to
keep a single process library in bbt: a double dependency, with two
execution engines and two sets of platform quirks to validate, is
worse than either option.

1. contribute the missing pseudo terminal configuration to Spawn
   (expose the slave descriptor, or provide a raw mode setting), and
   use it once released: one dependency, the feature delivered by
   construction, but gated on the upstream acceptance and release;

2. replace Spawn by ada-util entirely: one dependency, and the pseudo
   terminal comes already configured (raw mode), but the event pumping
   of the interactive steps has to be redesigned around blocking
   reads (no polling today: the underlying descriptor would have to
   be exposed upstream), and the Windows paths have to be validated;
3. keep filtering heuristics on top of the Spawn pipes (see above):
   no new dependency and no upstream work, but heuristics in the
   output path.

Using ada-util for the pseudo terminal scenarios only, keeping Spawn
for the others, is rejected: each library used where it is the best
fit would not compensate the cost of two process libraries in bbt.


_Table of Contents:_
- [Scenario: answering a C program that does not flush](#scenario-answering-a-c-program-that-does-not-flush)
- [Scenario: the line feeds are checked without the carriage returns](#scenario-the-line-feeds-are-checked-without-the-carriage-returns)
- [Scenario: the input echo is ignored](#scenario-the-input-echo-is-ignored)
- [Scenario: the simulated terminal has a standard size](#scenario-the-simulated-terminal-has-a-standard-size)

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

### Scenario: the simulated terminal has a standard size

A program querying its terminal size behaves as on a real terminal:
the pseudo terminal is given the standard 80x24 size, an unset size
(0x0) would be nonsense for the tested program.

- Given the new file `size.c` containing
  ```c
  #include <stdio.h>
  #include <unistd.h>
  #include <sys/ioctl.h>
  int main() {
    struct winsize ws;
    char c;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) != 0) {
      printf("no size\n");
    } else {
      printf("size %dx%d\n", ws.ws_col, ws.ws_row);
    }
    c = getchar();
    return 0;
  }
  ```
- When I successfully run `gcc size.c -o size`
- When I run `./size`
- Then the output is `size 80x24`
- When I type `q`
- Then I get no error
