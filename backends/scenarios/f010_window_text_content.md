# F010 — Window text content

Executable subset of the vocabulary feature F010 (window text
content check), run against the ToDoMVC witness app
(cf. docs/proposed/crate_restructuring.md, backends/scenarios).

The negative form (the window does not contain) and the footer
scenarios of [ToDoMVC](../../Apps/ToDoMVC/ToDoMVC.md) require
vocabulary steps not implemented yet; they are tracked in the
[GUI vocabulary proposal](../proposed/gui_vocabulary.md) and enter
this suite with the vocabulary absorption.

## Scenario: The creation line is in the window content

- Given there is no file `todos.state`
- When I run `./todomvc`
- Then the window contains `What needs to be done?`

## Scenario: The creation button is in the window content

- Given there is no file `todos.state`
- When I run `./todomvc`
- Then the window contains `Create`
