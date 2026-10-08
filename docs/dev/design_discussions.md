# Design discussions

This document holds the design discussions: each entry records a
significant design subject, with its status - under discussion, or
arbitrated - and, once arbitrated, the decision, the alternatives that
were rejected and why. When the discussion happened elsewhere (a pull
request, a GitHub discussion, a forum thread), the entry references it
and adds only the complements needed to understand the subject as it
stands.

An arbitrated entry may be superseded by a later entry; it is then
updated with a reference to its replacement.

| Subject                                                                                  | Status                             | References                                                                         |
|------------------------------------------------------------------------------------------|------------------------------------|------------------------------------------------------------------------------------|
| [D3. Readers and writers organization](#d3-readers-and-writers-organization)             | Under discussion                   | [markdown_utilities.ads](../../src/markdown_utilities.ads)                         |
| [D7. Status bar rendering](#d7-status-bar-rendering)                                     | Arbitrated (2026-10), implemented  | [B210_Status_Bar.md](../features/B210_Status_Bar.md)                               |
| [D5. Tests.Actions organization](#d5-testsactions-organization)                         | Arbitrated (2026-10), implemented                   | [bbt-tests-actions.ads](../../src/bbt-tests-actions.ads)                          |
| [D6. Scenario timeout](#d6-scenario-timeout)                                             | Arbitrated (2026-10), implemented           | [B200_Scenario_Timeout.md](../features/B200_Scenario_Timeout.md)                   |
| [D1. Command execution library: Spawn](#d1-command-execution-library-spawn)              | Arbitrated (2026-10)               | [spawn lib choice forum thread](https://forum.ada-lang.io/t/spawn-lib-choice/1467) |
| [D2. successfully and the interactive steps](#d2-successfully-and-the-interactive-steps) | Arbitrated (2026-10), implemented | [PR #40](https://github.com/LionelDraghi/bbt/pull/40)                              |
| [D4. Line endings: LF everywhere](#d4-line-endings-lf-everywhere)                        | Arbitrated (2026-10)               | [.gitattributes](../../.gitattributes)                                             |

The table is sorted by status: the entries under discussion first.

## D1. Command execution library: Spawn

Status: arbitrated (October 2026)

The commands of the `When I run` steps are executed through the
[Spawn](https://github.com/AdaCore/spawn) library (Alire dependency
`spawn`), asynchronously.

`GNAT.Expect` was rejected after experimenting with both (October 2026,
with scratch programs):

- `GNAT.Expect` (its `GNAT.Expect.TTY` child package is implemented on
  all native GNAT ports) exposes a consuming, sliding buffer: `Expect`
  returns the output up to the match, the tail arrives on the next call,
  and older data is discarded once `Buffer_Size` is reached. Mapping
  this pull-by-regexp model on the exact output checks of bbt would
  have required constant draining, and `Close` on an already terminated
  child reported a kill status (9) instead of its exit code. Moreover,
  on Unix `GNAT.Expect.TTY` sets up the pseudo terminal as a full
  terminal (echo, canonical mode and `CR` `LF` translation active, as
  gdb expects), while on Windows it provides pipes or a console, not a
  pseudo terminal: in both cases, the exact output checks of bbt would
  be polluted;
- `Spawn` (pipes) was validated byte exact on the two input forms: a
  key without newline received by `Ada.Text_IO.Get_Immediate`, a line
  with newline received by `Get_Line`, the prompt being flushed by
  `Ada.Text_IO` before the read. It is also the cross-platform path:
  Windows is implemented explicitly, and the library is used by Alire
  itself.

Other candidates were reviewed later (October 2026):

- `GLib.Spawn_Alt.Asynchronous`
  ([gtkada_contributions](https://www.dmitry-kazakov.de/ada/gtkada_contributions.htm)),
  proposed in the
  [spawn lib choice forum thread](https://forum.ada-lang.io/t/spawn-lib-choice/1467),
  and co-authored by the Spawn author: the pipes are serviced by
  dedicated tasks, the completion is notified once the process died and
  all pipes are closed, and the environment is passed at each run.
  Rejected: it drags the whole GTK+ dependency (`Gtk.Main.Router`, that
  requires a window) into a console tool, it is not distributed on
  Alire, and its callbacks run on separate tasks, which would force a
  protected object refactor of the current single task design. Spawn
  can be seen as the Alire packaged continuation of the same design,
  with a `spawn_glib` flavor when GLib main loop integration is wanted;
- the Alire index was swept for other process libraries: `spoon`
  (posix_spawn, no Windows), `ashell` (built on Florist, POSIX
  oriented, Windows availability dubious), `spawn_glib` (Spawn itself
  on the GLib event loop). None brings a feature bbt misses;
- `utilada` (`Util.Processes`, `Util.Streams.Pipes`) is the only
  credible alternative: on Alire, Apache-2.0, Windows supported, direct
  stdout and stderr redirection to files, and the `Set_Allocate_TTY`
  pseudo terminal option. It is the documented fallback for the pseudo
  terminal feature, cf.
  [proposed_features/pty.md](../proposed_features/pty.md), not a
  replacement for the current execution engine.

References:

- the [spawn lib choice forum thread](https://forum.ada-lang.io/t/spawn-lib-choice/1467);
- [AdaCore/spawn#36](https://github.com/AdaCore/spawn/issues/36): the
  POSIX monitor dangling pointer on freed process objects, that bbt
  works around by never freeing them (cf. the developer guide).

## D2. successfully and the interactive steps

Status: arbitrated (October 2026), implemented (October 2026)

`when I successfully run 'X'` is defined in
[A130](../features/A130_Successfully_Keyword.md) as the shortcut for
`When I run 'X'` followed by `Then I get no error`. Since the
interactive steps
([A290](../features/A290_When_I_Type_Or_Enter.md),
[PR #39](https://github.com/LionelDraghi/bbt/pull/39)) start a command
and feed it across steps, the command may still be running long after
its `run` step: what does "successfully" check then?

Three designs were considered:

1. forbid `successfully` on a run that is not completed in one step;
2. reduce `successfully` to check only that the command was launched;
3. keep the exact meaning of the shortcut, with a deferred check.

Design 2 is eliminated: the same keyword would silently mean two
things (the exit code, or a successful launch), and the launch check is
near useless: a test would pass while the command ultimately fails.

Design 1 is eliminated as the target: it forbids a real use case
(checking that an interactive command finally exits cleanly), and it
does not solve the problem, it moves it: an explicit
`Then I get no error` after the interactive steps raises the same
question. It remains acceptable as a transitional error, until design 3
is implemented.

Decision: design 3. `successfully` keeps its exact meaning: the exit
status check is deferred to the next `When I run` step, or to the end of
the scenario. bbt executes no run in parallel, so the check points are
deterministic. This generalizes to any exit status check
(`successfully`, `Then I get no error`): the check waits for the command
termination before being evaluated.

Complements, decided with the owner:

- the messages appear in the temporal order of their detection, and the
  exit status failure references the `successfully run` step line, so
  that the user understands which step causes the error;
- if, at the beginning of a step, the command has exited with an error
  since the previous step, the failure is reported on the
  `successfully` step and the current step is not executed: no
  misleading "no command is running" message;
- the deferred check at the end of a scenario may have to wait for the
  command termination: bounding the tests execution time is a parked
  proposal, cf. the to do list in
  [project.md](project.md#tdl).

References:

- [PR #40](https://github.com/LionelDraghi/bbt/pull/40): the design
  discussion, listing the three solutions and the elimination
  rationale.

## D3. Readers and writers organization

Status: under discussion

bbt reads Markdown (the MDG reader) and a close AsciiDoc subset, and
writes Markdown, AsciiDoc and text - the text writer being in fact a
second Markdown writer, as B140 shows: the verbose output is equal to
the Markdown index file.

The Markdown specific knowledge is scattered: the readers (mdg, adoc)
and the lexer know how to read, the writers know how to write, and
[Markdown_Utilities](../../src/markdown_utilities.ads) (2026-10)
centralizes the shared helpers (`Web_Path`, `Link`, `Checkbox`,
`Hard_Break`), but:

- `Text_Writer` duplicates the Markdown knowledge of
  `Markdown_Writer`: checkboxes, links, hard breaks, and the results
  summary table are written twice;
- `AsciiDoc_Writer` embeds Markdown conventions (the "  " hard break,
  whereas AsciiDoc uses " +");
- a future format that is not a Markdown flavor (reST, org mode...)
  would need its own reader and its own writer, and would reuse
  almost nothing of the existing writers.

Points to arbitrate:

- where does the abstraction line go between the writer hierarchy and
  the format utilities?
- should `Text_Writer` be a `Markdown_Writer`?
- should the markdown and asciidoc writers share a common intermediate
  representation?

The balance is between the factorization gain - one results model,
one writer hierarchy, the format syntax grouped per format utility
package - and the cost of an abstraction layer for a set of formats
that is, today, all Markdown derived.

References:

- [B140_Index_File.md](../features/B140_Index_File.md): the text
  writer output is the Markdown index;
- [markdown_utilities.ads](../../src/markdown_utilities.ads): the first
  factorization step.

## D4. Line endings: LF everywhere

Status: arbitrated (October 2026)

The repository uses LF line endings on every platform, Windows
included. This is enforced by [.gitattributes](../../.gitattributes)
(`* text=auto eol=lf`), and it generalizes the rule that already
existed for docs/help: a CR in the help files ends up in the text
embedded in bbt at build time, and in bbt's output checks.

The storage was already LF: the Windows working tree was the problem,
checked out in CRLF by core.autocrlf=true, a recurring source of tool
quirks and edits fighting over the file format.

The generated files, written by bbt itself, remain CRLF on disk on
Windows, as emitted by GNAT.Text_IO: git normalizes them at add, so
the storage stays LF and git status stays clean, and making bbt write
LF explicitly is not worth it.

This position stands as long as CRLF is not a requirement of a use
case, or the fix of a specific bbt bug.

References:

- the .gitattributes historical note on the docs/help precedent;
- [changelog.md](../changelog.md), 0.4.2-dev: CRs at line ends were
  significant in human match mode - the class of bugs the docs/help
  LF rule fixes.
## D5. Tests.Actions organization

Status: arbitrated (October 2026), implemented (October 2026)

`BBT.Tests.Actions` mixes in one package body (1728 lines) the
command machinery (Spawn listener, process and stream state, pumping,
`Run_Cmd`, `Send_Input`, the deferred exit status check), the file
and directory setup, the output and file content checks, the exit
code checks, and the environment management. The runner dispatches
them all from one case statement, itself already grouped by domain.

The precedent is set: `File_Operations` is already a child of the
private package, holding the low level file primitives. The proposal
is to continue on this pattern, one child per domain, mirroring the
branches of the runner case:

- `Actions.Commands`: the process machinery and its state (listener,
  process and stream state, `Running`, `Last_Exit_Code`, `Pump`,
  `Wait_*`, `Run_Cmd`, `Send_Input`, deferred exit status check,
  `Output_Since_Input`, `Reset_Interactive_State`);
- `Actions.Setup`: `Erase_And_Create`, `Create_If_None`,
  `Setup_No_File`, `Setup_No_Dir`, `Check_File_Existence`,
  `Check_Dir_Existence`, `Check_No_File`, `Check_No_Dir`;
- `Actions.Output_Checks`: `Output_*`, `Check_No_Output`, the
  `File_*` and `Files_Is*` checks - stateless, the checked text is
  a parameter;
- `Actions.Environment`: `Set_Env_Var`, `Unset_Env_Var`,
  `Restore_Environment`, and the environment memory;
- `File_Operations` stays as is.

The runner would with and use the children, its case statement
already matching the split. Two moves accompany the split:

- `Get_Expected`, a body helper invisible to the children, moves to
  `BBT.Model.Steps`: the expected content of a step is model
  knowledge, needed by `Setup` and `Output_Checks`;
- `Exit_Code_Is` takes the code as a parameter, as `Return_Error`
  already does, making the exit code checks stateless; the deferred
  check, that needs the command state, stays in `Actions.Commands`.

Rejected alternative: splitting `BBT.Tests.Actions` into sibling
packages visible from the whole application. It loses the
encapsulation of the private package, and breaks the precedent set
by `File_Operations`. The children of a private package keep the
test machinery invisible to the rest of bbt.

The move is mechanical: cut and paste plus with and use clauses, no
behavior change, to be verified by the unchanged test suites.

## D6. Scenario timeout

Status: arbitrated (October 2026), implemented (October 2026)

A test suite must stay CI friendly: a command hanging, looping
forever, or waiting for an input that no step provides must fail
fast with an explicit message, not block the whole pipeline. The
risk became acute with the interactive steps and the deferred exit
status checks (cf. D2), that may wait for a command termination at
the end of a scenario, possibly forever.

Decisions:

- the timeout is per scenario, for instance
  `--scenario_timeout <duration>`. Rejected alternatives: a timer
  per step, which does not fit a command spanning several steps, and
  where a hang may only show on the deferred checks; a timer for the
  whole run, which stops bbt after the damage is done, instead of
  failing at the place where the hang blocks, where bbt can name the
  culprit.
- the timer covers the scenario steps, backgrounds included, not
  the cleanup;
- on expiry, the still running command is killed, so that no
  process is left behind, and the scenario fails with a message on
  the hanging step line, so that the user understands which step
  causes the error; the deferred exit status check says that the
  timeout expired before the command termination, on the
  successfully run step line;
- the duration is a number of seconds with an optional unit:
  groups of digits followed by `s`, `m` or `h` are summed
  (`10`, `10s`, `2m`, `1m30s`); an invalid duration is a command
  line error;
- the default is no timeout at all, so that bbt's behavior is
  unchanged unless the option is explicitly given.

The enforcement is deadline based: the pumping loops
(`Pump`, `Wait_Quiet`, `Wait_Response`) exit when the deadline is
passed, and each step that can block on a command checks the
deadline and reports the failure. This bounds every wait on a
command, without a watchdog task and its abort hazards. The only
unbounded wait left is bbt's own confirmation prompt, that waits for
the user, not for a command.

References:

- the specification and the scenarios:
  [B200_Scenario_Timeout.md](../features/B200_Scenario_Timeout.md);
- the former proposal docs/proposed_features/timeouts.md, removed
  once implemented.

## D7. Status bar rendering

Status: arbitrated (October 2026), implemented

The `-sb | --status_bar` option displays a transient bar. Decisions:

- the bar lives on the current terminal line: BBT.IO erases it
  (`CR` + `EL 2K`) before any output on the standard output, and
  redraws it after a completed line, so that no residue is left
  over the normal output, and the bar never scrolls away;
- rejected alternative: writing at a fixed screen position
  (`CUP 1;1`), the former implementation, that overwrites the
  scrolled results and leaves fragments in the scrollback;
  also rejected: a dedicated line at the bottom of the screen,\  
  that requires the terminal height, unavailable in plain ANSI;
- during a run, the bar shows a spinner and a `<done>/<total>`
  counter counting scenarios as the final counters do: backgrounds
  are not counted, as their results are folded into each scenario,
  and filtered scenarios are counted, as they are visited and end
  up Not Run; the spinner is time based (twelve frames per second,
  indexed on the clock): advancing it at each step gave a jerky
  animation, the steps having very uneven durations, and frames
  skipped by the eye; the glyph is still sampled by the redraws, so
  a background timer remains the only way to animate during a long
  silent command (cf. the TDL in project.md);
- the bar is written directly through Ada.Text_IO, never through
  BBT.IO, so that it never ends up in the tee file, and the
  erase/redraw protocol cannot recurse;
- the one shot commands (listings, help, version) do not use the
  bar: they are instantaneous, and most of their output bypasses
  BBT.IO;
- the spinner is deliberately ASCII (`|/-\`), to stay readable on
  any terminal; when the terminal renders UTF-8, a braille spinner
  and pass/fail glyphs (green check mark, red cross) replace it:
  the Unicode level comes from termicap, and, on Windows, the
  console output code page is checked to be 65001 before emitting
  any non ASCII glyph (Status_Bar.Platform, whose body is selected
  per host OS in bbt.gpr): a terminal hosting a code page such as
  850 would otherwise display mojibake;
- the bar is displayed only when the standard output is a terminal,
  detected through termicap (`Termicap.Capabilities.Get`): on a
  redirected output, it would only fill the logs with escape
  sequences; the `--force_status_bar` option overrides this: it is
  a debugging option, undocumented in the help on purpose, used by
  the feature tests themselves, that run the nested bbt through
  pipes;
- the bar colours are sober: no background, gray text, a cyan
  spinner, and a green check mark / red cross on the last scenario
  outcome, the palette being selected according to the colour level
  detected by termicap (nothing at all under `NO_COLOR`, which the
  termicap detection honours, the 16 standard colours on a basic
  terminal, fine RGB otherwise), so that the bar stays readable on
  light and dark themes alike;
- as soon as a scenario fails, the whole bar turns red, and
  displays the failure count until the end of the run: the ticks of
  the following scenarios, and their green check marks, cannot erase
  the failure from the user's view.

Known limitation: the spinner is frozen while a single command
runs for a long time; a background task refreshing the bar on a
timer would remove this, at the cost of serializing the writes
on the standard output (cf. the TDL in project.md).

Known refactoring candidate: the `with BBT.Status_Bar` in
bbt-io.adb inverts the dependency direction: the base output
layer depends on a UI component, that conceptually should depend
on it. The clean fix is an output hook: BBT.IO would expose a
registration (a pair of access-to-procedure, or a dispatching
observer), and the status bar would register its Clear / Draw at
Enable, restoring the natural direction, BBT.IO knowing nothing
about bars. Kept as is for now: the coupling is minimal (two
procedure calls in the stdout paths), and it cannot recurse, the
bar writing through Ada.Text_IO directly, never through BBT.IO;
revisited if the IO package gets other output companions, or
when the writers organization (D3) is arbitrated.

References:

- the specification and the scenarios:
  [B210_Status_Bar.md](../features/B210_Status_Bar.md);
- the implementation: bbt-status_bar.adb, and the erase/redraw
  hooks in bbt-io.adb.
