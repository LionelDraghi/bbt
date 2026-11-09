# ToDoMVC

A todo-list application used as a GUI witness app: a real, everyday
application with enough statefulness (list, edition, filters,
persistence) to exercise a GUI vocabulary end to end. Every bbt
backend implementation (see `web/`, `gtk4/`, `adi2/`) must satisfy this
specification, and its scenarios are the executable form of the
requirements below.

The functional requirements are derived from the
[TodoMVC application spec](https://github.com/todomvc),
chapter *Functionality*. The GUI vocabulary requirements they exercise
(launch, window and widget lookup, state, press...) are defined in
`docs/specifications.md`, section GUI: this application is their
witness, not a requirement source.

## Widget naming contract

The scenarios rely on semantic widgets only — role + accessible name —
never on positions or pixels. Each implementation must expose:

| Widget | Role | Accessible name |
|---|---|---|
| Todo input | textbox | `What needs to be done?` |
| Todo creation | button | `Create` (single) |
| Mark-all control | button | `Mark all as complete` (footer, with the purge) |
| Item completion toggle | button | `Complete` (one per todo item) |
| Item editing trigger | button | `Edit` (single, on the selected todo) |
| Item delete control | button | `Delete` (single, on the selected todo) |
| Item editing input | textbox | `Edit todo` |
| Completed purge | button | `Clear completed` |
| Filters | toggle button | `All`, `Active`, `Completed` |

Exactly one todo at a time can be selected, by activating (clicking)
its title; the selected todo is highlighted in the list, and the
selection moves by activating another title. The `Edit` and `Delete`
controls apply to the selected todo, and are insensitive (grayed) as
long as the selection is empty.

The layout, per implementation: the filters sit at the top of the
window, above the list, right under the creation line; the footer sits
at the bottom, and holds the mark-all and purge controls together,
side by side, on the same line, with the active counter.

The todo items themselves appear in the window content; a completed
todo is rendered with its title struck through in implementations with
rich text, and reflected by the `Completed` filter in all of them.

Note: the reference spec drives item edition with a double click and
deletion with a hover-revealed button. These pixel-level interactions
have no equivalent in a semantic vocabulary, so each implementation
exposes the selection and the equivalent explicit controls listed
above instead: no per-item Edit or Delete button, no duplicated item
name in the accessible names.

## Functionality

### No todos

When there are no todos, the header (creation line, filters) and the
footer (mark-all, counter, purge button) are hidden.

```bbt
### Scenario: fresh app shows no list and no footer
- When I run `./todomvc`
- Then a window appears
- And the window does not contain "items left"
- And the window does not contain "completed"
```

### New todo

Todo text is entered in the `What needs to be done?` textbox, which has
the focus when the app starts. Validating the entry (the `Create`
button, standing for the Enter key of the reference spec) appends the
todo to the list and clears the input. Text is trimmed, and empty text
is not accepted.

```bbt
### Scenario: create a todo
- When I run `./todomvc`
- And I type "buy milk" into the `What needs to be done?` textboxcompleted
- And I press the `Create` button
- Then the window contains "buy milk"

### Scenario: empty entry is rejected
- When I run `./todomvc`
- And I press the `Create` button
- Then the window does not contain "items left"
```

The `type into` step form belongs to the GUI vocabulary currently
being implemented (cf. `docs/proposed/gui_vocabulary.md`); the backend
directories provide the per-backend runnable versions of these
scenarios.

### Footer: mark all and purge

The footer sits at the bottom of the window, and holds the mark-all
and purge controls together, side by side, on the same line.

The `Mark all as complete` control toggles all the todos to the same
state as itself. It must stay up to date with the items: when every
todo is checked, so is `Mark all as complete`. Completing everything
also makes the counter show zero active items. Once the completed
todos are purged, the mark-all control goes back to unchecked.

The `Clear completed` button purges the completed todos and updates
the counter. When there is no completed todo, the button is not shown.

### List

The todo list is visually set off from the surrounding window
content: it sits in a frame (a hollow box) of the same width.
The todo title text is justified to the left within its item.
The todos appear in their creation order, whatever their completion
state; a completed todo is green, a todo still to be done is blue
(the colors are per-implementation, as long as the two states stay
visually distinct in the default theme).

### Item

Each todo item can be selected: activating its title selects it, the
selected todo is highlighted, and the unique `Edit` and `Delete`
controls (see the widget naming contract) apply to it. The item's
completion toggle is the single `Complete` button: it applies to the
selected todo, like `Edit` and `Delete`. Marking a todo complete
updates its rendering and its filter membership, and decrements the
active counter.

### Editing

Editing is started with `Edit` on the selected todo, and replaces the
item rendering by an edit input (`Edit todo` textbox), focused,
holding the current title. The edit is saved on *validate* and on
leaving the field, trimmed; an edit saved as empty destroys the todo.
Pressing `Edit` again while the edition is open cancels it, restoring
the previous title untouched.

### Counter

The footer displays the number of **active** todos, correctly
pluralized: `0 items left`, `1 item left`, `2 items left`.

```bbt
### Scenario: counter pluralization
- When I run `./todomvc`
- And I type "a" into the `What needs to be done?` textbox
- And I type "b" into the `What needs to be done?` textbox
- Then the window contains "2 items left"
```

### Persistence

Todos survive a restart, with their completed state, in the
implementation storage; an edition in progress is not persisted
(a restart discards an unfinished edition).

```bbt
### Scenario: todos survive a restart
- When I run `./todomvc`
- And I type "buy milk" into the `What needs to be done?` textbox
- And I close the application window
- When I run `./todomvc`
- Then the window contains "buy milk"
```

(The `close the application window` step closes the main window,
i.e. a clean quit of the app, not a kill: the todo must already be in
the storage when the window goes away. Closing the window explicitly
before relaunching makes the two runs independent — no implicit
"the backend reaps the previous instance" hand-waving.)

### Routing

The filters `All` (default), `Active` and `Completed` sit at the top
of the window, above the list, right under the creation line. They
show the matching subset of todos. The selected filter is visible in
the interface — the filter state is displayed, and the filter buttons
are toggles: the selected one shows the checked aspect, its toggle
state, while `All` is unchecked by default. The active filter survives
a restart, and an item
updated so that it no longer matches the current filter disappears
from the list.
