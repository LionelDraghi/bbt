# GUI features <!-- omit from toc -->

This page specifies the GUI vocabulary of bbt, series `Gnnn`; the
pending vocabulary stays in [proposed](proposed/), tracked by
[specifications.md](specifications.md). Each entry links to the user
story scenarios that exercise it: the stories live in the shared
witness suite
([Apps/ToDoMVC/docs/features](../Apps/ToDoMVC/docs/features)) —
one scenario launches the app once and checks everything the
implemented vocabulary allows, the exercised steps tagged `[Gnnn]`.
The per-step backend status is tracked in the
[GUI capability matrix](gui_matrix.md).

## Contents <!-- omit from toc -->
- [G005 — Launch and window observation](#g005--launch-and-window-observation)
- [G010 — Text content of a window](#g010--text-content-of-a-window)
- [G015 — Button by accessible name, with enabled state](#g015--button-by-accessible-name-with-enabled-state)
- [G020 — Press action on a control](#g020--press-action-on-a-control)
- [G035 — Type text into a field, and enter it](#g035--type-text-into-a-field-and-enter-it)
- [G080 — List structure, with content and emptiness checks](#g080--list-structure-with-content-and-emptiness-checks)
- [G095 — Window close and cross-scenario application lifetime](#g095--window-close-and-cross-scenario-application-lifetime)

#### G005 — Launch and window observation

*Source: `examples/reference-scenarios.md` (welcome interface),
milestone 1.*

Need: launch the application under test and observe that a window
exists, optionally by name — no scenario can run without it.

- `When I run <command>`: launch the application under test; a run
  step closes any previous instance first (the relaunch semantics:
  only its own window remains);
- `Then a window appears` / `Then a window named <name> appears`.

Grounding: ARIA has no role for a native top-level window (the root
of the accessibility tree is the document); `window` is therefore a
bbt canonical concept, mapped per backend (Web: the page and its
document; GTK: the window/frame role) — concerns Q001, Q002. It also
fixes the launch/observe boundary between the runner core and a
backend (Q005).

Verified by: every witness story opens with a launch; the suite as
a whole exercises the relaunch semantics, and the two named forms are
exercised by the first scenario of
[edit_or_delete_a_task.md](../Apps/ToDoMVC/docs/features/edit_or_delete_a_task.md).
Open questions: Q001, Q002.

#### G010 — Text content of a window

*Source: `examples/reference-scenarios.md` (welcome interface),
milestone 1.*

Need: after launch, the first assertion on content — the window
shows the expected text.

- `Then the window contains <text>`: text content assertion, scoped
  to the window.

Grounding: text content is the rendered text of the subtree,
distinct from the accessible name and description computed per
[accname](https://www.w3.org/TR/accname-1.2/); the backend reads the
accessible text of the document subtree, and no DOM or CSS concept
leaks into the scenario. A control is found by name (G015); a text
assertion reads content.

Verified by:
[add_a_task.md](../Apps/ToDoMVC/docs/features/add_a_task.md): the task
the scenario creates is asserted in the window text, and a task it
never created is asserted absent, the negated form. The footer counter
("3 items left") stays out of the executable subset so far: no story
needs it, and the two lists (G080) carry the same information without
depending on how a toolkit renders a counter.

#### G015 — Button by accessible name, with enabled state

*Source: `examples/reference-scenarios.md` (welcome interface),
milestone 1.*

Need: the first widget lookup — find a control by role and
accessible name, and assert its enabled state.

- `there is a button named <name>`;
- `the <name> button is enabled` / `is disabled`.

Grounding: role
[button](https://www.w3.org/TR/wai-aria-1.2/#button); the name is
computed per accname (the scenario never sees the attribute used);
enabled is the absence of
[aria-disabled](https://www.w3.org/TR/wai-aria-1.2/#aria-disabled) —
the vocabulary exposes the user-visible state, not the attribute.
This fixes the lookup key: role + accessible name (concerns Q010 to
Q012).

Verified by:
[add_a_task.md](../Apps/ToDoMVC/docs/features/add_a_task.md),
[complete_a_task.md](../Apps/ToDoMVC/docs/features/complete_a_task.md),
[edit_or_delete_a_task.md](../Apps/ToDoMVC/docs/features/edit_or_delete_a_task.md).
Open questions: Q010, Q011, Q012.

#### G020 — Press action on a control

*Source: `examples/reference-scenarios.md` (continue to login),
milestone 1.*

Need: the first action, from observation to acting on the
application — without one semantic action, nothing can be driven.

- `When I press the <name> button`: semantic activation of a button;
- `When I select the <name> listitem`: selecting an item of a list —
  the todo a scenario then edits or deletes in the ToDoMVC witness;
- `When I press the <name> listitem`: the synonym, same action, same
  rule, for the items a user would rather say they press; the suite
  exercises it once.

Same verb behind the scenes (both set `press`), so a scenario says
what its reader understands: `select` for an item, `press` for a
button. The wording is the user's, not the specification's —
"activate" is jargon, and "press" on a row leaves the gesture open
(pressed while the button is held, released after?), whereas the row
changing colour is exactly what "selected" says.

Grounding: ARIA declares roles, states and properties, never
actions; what maps them onto the platform layer is
[Core-AAM](https://www.w3.org/TR/core-aam-1.2/), the W3C
Accessibility API Mappings — per accessibility API (MSAA/IAccessible2,
UIA, ATK/AT-SPI, macOS AX), each role with its state and property
mapping, and with them the patterns that carry the actions. So there
is no "press" or "select" in ARIA to be conformant with: each backend
maps the verb to its native activation (Web: the element's default
action; GTK/AT-SPI: the `DoAction` interface, and the action named
`select` that a GTK 4 list row exposes; UIA: `Invoke`, or
`SelectionItem` where the widget carries a selection). A device-level
`click` is deliberately kept for a later candidate feature. This is
the first entry in the action tier model (semantic action vs system
input) that the architecture and the CI strategy rely on.

Two things are worth knowing before inventing more verbs:

- *selection is a state in ARIA* — `aria-selected`, on `gridcell`,
  `option`, `row` and `tab`, inherited into `columnheader`,
  `rowheader` and `treeitem`; a `list`/`listitem` pair is a static
  list, and the selectable collection of the Authoring Practices is
  `listbox`/`option` (or `grid`/`row`). A `listitem` therefore
  carries no state a backend could assert, which is why `select`
  here is a wording, not a check — the state predicate is G045's
  (`is selected`), and the witness may have to expose a
  `listbox`/`option` pair for the selection to be ARIA-visible;
- *the platform layer separates the two actions*: the UIA
  `SelectionItem` pattern (`Select`, `IsSelected`) against the
  `Invoke` pattern, the AT-SPI `Selection` interface and the action
  named `select` of a GTK 4 list row, against `DoAction` on a
  button; the "activate" of the Authoring Practices is the default
  action of the widget, whatever the pattern behind it.

Verified by:
[add_a_task.md](../Apps/ToDoMVC/docs/features/add_a_task.md),
[complete_a_task.md](../Apps/ToDoMVC/docs/features/complete_a_task.md),
[edit_or_delete_a_task.md](../Apps/ToDoMVC/docs/features/edit_or_delete_a_task.md)
(the item selected in every scenario, plus the one `press` of the
synonym). Open questions: Q008 (automatic fallback between action
levels).

#### G035 — Type text into a field, and enter it

*Source: ARIA 1.2 value roles.*

Need: observing fields is not enough — scenarios must set text
values, and commit them the way a user commits them.

- `When I type <text> into the <name> textbox`: a semantic value set
  (the ARIA textbox accessible value; AT-SPI: the EditableText
  SetTextContents), with **no validation side effect**;
- `When I enter <text> into the <name> textbox`: the same value set,
  followed by the validation — the A290 CLI rule (`enter` = `type` +
  line terminator) carried over to the GUI.

The commit **cannot** be the AT-SPI `activate` action, a silent
no-op on GTK 4 (dispatched to `gtk_widget_activate`, result
discarded; `GtkEntry` sets no activation signal): the commit is a
`Return` keyboard synthesis — a virtual-keyboard tier input, not a
semantic action — whose carrier is a backend design choice, decided
per windowing stack. Typing targets role textbox; the value widgets
(slider, spinbutton) stay proposed (G085, G090).

Verified by:
[add_a_task.md](../Apps/ToDoMVC/docs/features/add_a_task.md)
(its `type` and `enter` steps). Open questions: Q018 (UTF-8, IME for
non ASCII entries); upstream report of the GTK no-op `activate`
action.

#### G080 — List structure, with content and emptiness checks

*Source: ToDoMVC witness design session (2026), milestone 1.*

Need: verify structured collections (task lists, result tables) by
role and accessible name — without it, a filled "To do" or "Done"
list is indistinguishable from stray labels in the window text.

    - Then there is a list named `To do`
    - Then the `To do` list is empty
    - And the `To do` list has `3` items
    - And the `To do` list contains `Buy milk`
    - And the `Done` list does not contain `Buy milk`

- `there is a <name> list`: the role-word form of `there is a <role>
  named <name>` — it reuses the same lookup machinery;
- `the <name> <role> is empty`: nothing to see, in the role's own
  terms — no text for a `textbox` or a `listitem`, no item for a
  `list`, whatever else the widget may hold: a `textbox` decorated
  with a clear button, or a container keeping invisible placeholder
  children, is still empty for the user;
- `the <name> <role> contains <text>`: rendered text in the widget
  subtree, the negated form reporting the inverted check;
- `the <name> <role> has <n> items`: the child count of the accessor
  (AT-SPI child count; ARIA has no list size property, its
  `aria-setsize` is carried by each item);
- the `is empty` and `contains` forms are the first GUI steps with
  the widget in the subject phrase (like "the `Edit` button is
  disabled"): the widget name comes from the subject, the
  expectation from the object.

Grounding: roles
[list](https://www.w3.org/TR/wai-aria-1.2/#list) and
[listitem](https://www.w3.org/TR/wai-aria-1.2/#listitem); GTK 4 maps
GtkListBox to AT-SPI role list (31), rows to list item (32), the row
content to the row label's accessible name — the backend walks that
same structure.

Verified by:
[add_a_task.md](../Apps/ToDoMVC/docs/features/add_a_task.md),
[complete_a_task.md](../Apps/ToDoMVC/docs/features/complete_a_task.md)
(the two witness lists, "To do" and "Done", are the fixture). Open
questions: Q013 (the deadlines are shared, the same as the window
checks); and the implementation gap below.

`is empty` is a user-facing statement, not a count of children: the
emptiness is the one of the role at hand. A `textbox` holding no
text is empty even when it also exposes a button in its corner; a
`list` with no item is empty, whatever the toolkit keeps under the
widget; a `button` is neither empty nor full of anything, it is
named, so the qualifier means nothing there. Both backends
implement it today as "no child under the widget", for every role —
right for the witness lists, wrong for the fields (a filled
`textbox` reads empty) and meaningless for a `button`. The intended
scope is thus per role family, with a (qualifier, role) pair that
means nothing failing with a diagnostic instead of answering.
Nothing is implemented nor decided yet: the step accepts every role,
and only the two witness lists are exercised (cf.
[gui_matrix.md](gui_matrix.md#roles)).

#### G095 — Window close and cross-scenario application lifetime

Need: killing the application at every scenario boundary makes
stories slow (a full launch plus a GTK accessibility registration
per scenario) and breaks chaining — the scenarios of one document
share one instance, closed explicitly by the story itself.

- `When I close the window`: close the application's only window;
  the step refuses (diagnostic) if the application exposes several,
  since the name disambiguates;
- `When I close the <title> window`.

Lifetime (runner level): `When I run` still closes any previous
instance first (the G005 relaunch semantics); the runner no longer
shuts the application down at scenario boundaries — one scenario and
the next observe the same instance; the end-of-run shutdown remains
the safety net. A story closes its windows explicitly, normally at
the end of the last scenario of its document.

Close (backend level): resolve the target window accessible (by
title, or the application's only frame), trigger the AT-SPI action
named `window.close` — by name, never by index, the window action
set being toolkit-version dependent — wait for the window to leave
the accessibility tree within the harness deadline, and reap the
process if the application ends with its window; a stack with no
close action, or a refused one, gets the process-level fallback,
reported. The close is graceful: the application's own close path
runs, persistence hooks included.

Verified by: the `close the window` steps closing every witness
story, the by-title form closing the last scenario of
[edit_or_delete_a_task.md](../Apps/ToDoMVC/docs/features/edit_or_delete_a_task.md),
and the chained scenarios of
[add_a_task.md](../Apps/ToDoMVC/docs/features/add_a_task.md)
(several scenarios on one launch, cross-scenario lifetime).

Open questions: close with no such window is an error — the step
waits and verifies, a reverse `the <title> window is closed`
assertion is deferred; an application that survives its last window
(windowless main loop) is out of the step's scope, it closes
windows, not applications; whether a document-boundary safety net
is needed before the end-of-run one.
