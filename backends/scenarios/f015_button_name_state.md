# F015 — Widget by accessible name, with enabled state

Executable subset of the vocabulary feature F015 (widget lookup,
enabled state), run against the ToDoMVC witness app
(cf. docs/proposed/crate_restructuring.md, backends/scenarios).

Per the [widget naming contract](../../Apps/ToDoMVC/ToDoMVC.md),
`Edit` and `Delete` apply to the unique selected todo and are
insensitive while the selection is empty; with no todo yet, they are
the only fixtures of the disabled state.

## Scenario: The creation line widgets are exposed

- Given there is no file `todos.state`
- When I run `./todomvc`
- Then there is a textbox named `What needs to be done?`
- And there is a button named `Create`

## Scenario: The fresh app leaves the selection actions disabled

- Given there is no file `todos.state`
- When I run `./todomvc`
- Then the `Edit` button is disabled
- And the `Delete` button is disabled
