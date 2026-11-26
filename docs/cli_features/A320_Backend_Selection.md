<!-- omit from toc -->
## Feature : GUI backend selection

A GUI step is served by a backend executable, `bbt_<kind>`, which the
`bbt` programme launches on demand: `--backend=<kind>` replaces the bbt
process with the backend executable found in the `PATH`, forwarding the
remaining arguments unchanged, so that the scenario and its options
reach the backend untouched. The GUI vocabulary, its steps and its
grammar stay in the core: the backend only provides the toolkit access
(cf. [D16](../dev/design_discussions.md#d16-one-repository-several-crates-the-gui-backends)
and [backends.md](../backends.md)).

Two early failures make the missing piece actionable, and both happen
before any scenario runs, so a GUI document never fails halfway on a
machine without a backend:

- a document containing GUI steps, with no backend executable invoked
  and none registered in the core;
- an explicit `--backend=<kind>` naming a kind whose executable is not
  in the `PATH`.

Both are execution preconditions, not parsing ones: a backend-less
machine still analyses a GUI document, and `explain` rebuilds its GUI
steps, which is what one runs first to diagnose the missing piece.

_Table of Contents:_
- [Scenario: a GUI step without any backend aborts with a selection hint](#scenario-a-gui-step-without-any-backend-aborts-with-a-selection-hint)
- [Scenario: a missing backend executable gives an install hint](#scenario-a-missing-backend-executable-gives-an-install-hint)
- [Scenario: explain rebuilds a GUI document with no backend](#scenario-explain-rebuilds-a-gui-document-with-no-backend)

## Scenario: a GUI step without any backend aborts with a selection hint

The document is analysed before it runs: the GUI step is recognised by
the core grammar, no backend is registered, and the run stops on one
line telling which option selects one.

- Given the file `gui.md` containing
~~~
## Scenario: window check

- Then a window appears
~~~
- When I run `./bbt gui.md`
- Then the output is `Error: no GUI backend available: select one with --backend=<kind>`
- And the exit code is `1`

## Scenario: a missing backend executable gives an install hint

The kind is valid, the executable is not there: the message names the
backend programme and the `alr` command that installs it, instead of a
process launch failure or a silently skipped step. The scenario run
through the option is a plain CLI one, which shows that the check comes
from the option, not from the steps.

- Given the file `scen.md` containing
~~~
## Scenario: trivial

- When I run `echo hello`
- Then the output contains `hello`
~~~
- When I run `./bbt scen.md --backend=nosuch`
- Then the output is `Error: bbt_nosuch executable not found in PATH; install it with: alr install bbt_nosuch`
- And the exit code is `1`

## Scenario: explain rebuilds a GUI document with no backend

Reconstructing a document is analysis, so the backend precondition does
not apply: the GUI step is printed as the grammar understood it, and
the command succeeds.

- Given the file `gui.md` containing
~~~
## Scenario: window check

- Then a window appears
~~~
- When I run `./bbt explain gui.md`
- Then the output contains `Check that a window appears`
- And the exit code is `0`
