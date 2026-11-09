# GUI testing vocabulary with native execution <!-- omit from toc -->

Source: design discussion with the project owner (2026): "je veux que
bbt soit autonome autant que possible, et fasse pour les GUI ce qu'il
fait pour les CLI".

## Need

bbt is a black-box test tool for CLI programs. Testing a GUI program
today means leaving bbt: the scenario loses the ability to assert
against the application's rendered interface, and the harness cannot
drive it or observe it. The tooling should let a bbt scenario launch a
GUI application, drive it, and assert on what the user perceives — the
same way bbt launches a CLI program and asserts on its output.

The vocabulary and the semantics follow WAI-ARIA concepts (role,
accessible name, state), so that a scenario asserts on what the user
perceives through the same grammar engine as the CLI one.

## Intended outcome

A bbt scenario can contain GUI steps mixed with the existing CLI steps:

```markdown
## Scenario: Calculator addition

- Given the file `calc.ini` containing `mode=basic`
- When I run `witness`
- Then a window appears
- And there is a button named `Continue`
- When I press the `Continue` button
- Then the window contains `Result: 42`
```

The GUI steps are parsed by the same grammar engine, dispatched by the
same runner, and executed through a GUI access layer specific to the
running platform (Linux first, via AT-SPI / libatspi).

## Scope

- Grammar: the step grammar gains the GUI vocabulary below. The parse
  engine stays unique; GUI rules live in their own rule set, so that a
  vocabulary extension never perturbs CLI parsing.
- Model: `Step_Data` gains GUI payload fields (role, accessible name,
  state), used only by GUI actions.
- A new execution layer abstracts GUI backends: first AT-SPI on Linux,
  then a webdriver backend for web applications. The backend is
  selected on the command line, not in
  the scenario (`--backend=atspi` on the command line, with
  `BBT_BACKEND=atspi` as the usual environment variable fallback).
- A second candidate backend: the **Adi2**
  backend (Adi.MCP development bridge), which observes and drives a
  probe-instrumented application over a file IPC session namespaced
  by the launched pid. Its plain pid correlation and its
  non-a11y transport make it a natural fit for the `--headless` CI
  option.
- A `--headless` option runs GUI steps in CI without an interactive
  display, on the backends where it is possible (headless browser for
  web, virtual display or hidden accessibility bus for AT-SPI), with a
  clean error where it is not.
- The existing CLI behavior is untouched: scenario files with no GUI
  step must not change in any observable way.

### MVP vocabulary

Aligned with the MVP sets prototyped in the older GUI test projects,
which bbt scenarios already resemble:

| Step | Meaning |
|---|---|
| (`When`) I run `app` | launch the GUI application (existing `run` mechanic, plus the accessibility environment setup) |
| Then a window appears / a `name` window appears | a top-level window of the launched app exists |
| Then there is a `role` named `name` | role + accessible name lookup (role from the frozen MVP set: button, textbox, dialog, ...) |
| Then the `name` `role` is enabled / disabled | user-visible widget state |
| When I press the `name` `role` | semantic activation (AT-SPI action, not injected input) |
| When I type `text` into the `name` `role` | text entry |
| Then a `name` dialog eventually appears | bounded wait; the deadline is configuration, the failure diagnostic carries it |

## Non-goals

- Mouse-position driven interaction, pixel comparison, screenshots
  (GUI-tool territory; a later proposal).
- Full ARIA vocabulary (later milestones: landmarks, tables,
  drag and drop, key events). This proposal only opens the way.
- Browser/webdriver backends.
- Any change to CLI-only scenario behavior.

## Acceptance criteria

- A bbt scenario mixing CLI steps and GUI steps parses, runs, and
  reports on each step.
- On a machine with no accessibility service, a GUI step fails with a
  clean diagnostic naming the missing backend, not an exception.
- `--backend=<backend>` selects the GUI backend; requesting a missing
  backend fails cleanly and names what is missing.
- With `--headless`, GUI scenarios run on headless CI runners for the
  backends that allow it, and report a clean "not supported" elsewhere.
- `bbt explain` documents GUI steps; `bbt help grammar` and
  `bbt help keywords` list them.
- The bbt functional suite still passes unchanged on CLI scenarios,
  including on headless CI runners when no GUI scenario is selected
  (GUI scenarios are excluded when no backend is available, like
  `Windows_Only` and `Unix_Only` tags).

## Executable scenarios

<!-- Load the bbt skill before editing this section. -->

### Scenario: no-backend diagnostic

- Given the file `gui_scenario.md` containing a GUI step
- When I run `bbt` with the no-backend environment
- Then the output contains `no GUI backend available`
- And the output contains the step location

### Scenario: launch, press, assert

- Given the file `gui_scenario.md` with `When I run` `witness` and
  `Then there is a button named "Continue"` with a pressed button step
- When I run `bbt gui_scenario.md`
- Then the only scenario step result is passed

(The applications under test are small witness programs, launched by
the backend like any scenario binary; their accessibility environment
setup is part of the backend's job, not the scenario's.)

## Design implications

- Step parsing: unique engine, two rule sets (CLI rules, GUI rules)
  registered after grammar init; registry becomes sparse (rule
  vector/map) rather than a dense 5-D array, so that GUI keywords stop
  multiplying dense-array cells.
- Keywords: the lexer keyword list stops being a single flat list;
  keywords become registered data attached to the rule set that needs
  them.
- Model: `Step_Data` gains GUI fields (role, accessible name, widget
  state) — still one record, no variant type.
- Runner: the GUI backend choice is a runner setting, like the
  platform; `BBT.Tests.Runner` dispatches GUI actions to it.
- The backend API surface is launch, poll with bounded deadline,
  shutdown; bbt has its own Ada binding.
  
## Open questions

- Backend-first order (AT-SPI only at first, webdriver later).
- How best to freeze the MVP role set, and how to report
  an unknown role.
- Whether `eventually` becomes a general step qualifier (retry with
  deadline) usable on CLI steps too, or stays GUI-specific.
