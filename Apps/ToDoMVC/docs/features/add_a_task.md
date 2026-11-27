# Story: The user adds a task

The creation line is exposed and named per the [widget naming
contract](../../ToDoMVC.md). The add itself: the `type` step sets the
textbox value (semantic, no validation side effect), `enter` commits
it (the creation line's Enter), and so does pressing `Create`. The new
task lands in the `To do` list, and the `Done` list stays empty.

(specs: [G005](../../../../docs/gui_features.md#g005--launch-and-window-observation),
[G015](../../../../docs/gui_features.md#g015--button-by-accessible-name-with-enabled-state),
[G020 press](../../../../docs/gui_features.md#g020--press-action-on-a-control),
[G035 type](../../../../docs/gui_features.md#g035--type-text-into-a-field-and-enter-it),
[G080 list structure](../../../../docs/gui_features.md#g080--list-structure-with-content-and-emptiness-checks))

## Scenario: The creation line is ready for a new task

- Given there is no file `todos.state`
- When I run `./todomvc`
- Then a window appears
- And there is a textbox named `New task` [G015]
- And there is a button named `Create`
- And there is a list named `To do` [G080]
- And the `To do` list is empty
- And there is a list named `Done`
- And the `Done` list is empty

## Scenario: The user adds a task

The application launched by the previous scenario is still running:
the scenarios of a story chain on the same instance (G095).

- When I type `Buy milk` into the `New task` textbox [G035]
- And I press the `Create` button [G020]
- Then the `To do` list contains `Buy milk`
- And the window contains `Buy milk` [G010]
- And the window does not contain `Book the dentist`
(those two steps are just tests of the "windows contains" form, testing directly the todo list is more precise)
- And the `Done` list is empty

## Scenario: The user adds several tasks

- When I enter `Water the plants` into the `New task` textbox
- Then the `To do` list contains `Water the plants`
- When I enter `Book the dentist` into the `New task` textbox
- Then the `To do` list contains `Book the dentist`
- And the `Done` list is empty

The story is done with the application: its window is closed
explicitly (G095).

- When I close the window

