# Design decisions

This document is bbt's decision log: it records the significant design
decisions, their context, and the alternatives that were rejected. When
the discussion that led to a decision happened elsewhere (a pull
request, a GitHub discussion, a forum thread), the entry references it
and adds only the complements needed to understand the decision as it
stands.

An entry may be superseded by a later entry; it is then updated with a
reference to its replacement.

_Table of Contents:_
- [D1. Command execution library: Spawn](#d1-command-execution-library-spawn)
- [D2. successfully and the interactive steps](#d2-successfully-and-the-interactive-steps)

## D1. Command execution library: Spawn

Status: accepted (October 2026)

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
  [proposed_features/pty.md](proposed_features/pty.md), not a
  replacement for the current execution engine.

References:

- the [spawn lib choice forum thread](https://forum.ada-lang.io/t/spawn-lib-choice/1467);
- [AdaCore/spawn#36](https://github.com/AdaCore/spawn/issues/36): the
  POSIX monitor dangling pointer on freed process objects, that bbt
  works around by never freeing them (cf. the developer guide).

## D2. successfully and the interactive steps

Status: accepted (October 2026), implementation pending

`when I successfully run 'X'` is defined in
[A130](features/A130_Successfully_Keyword.md) as the shortcut for
`When I run 'X'` followed by `Then I get no error`. Since the
interactive steps
([A290](features/A290_When_I_Type_Or_Enter.md),
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
