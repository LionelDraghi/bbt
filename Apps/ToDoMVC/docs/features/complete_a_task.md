# Story: The user completes a task

Per the [widget naming contract](../../ToDoMVC.md), completing a todo
moves it from the `To do` list to the `Done` list, and the purge
removes the completed todos for good. A single todo is completed by
selecting it (with the `select` step — the activation action on the
item, G020) and pressing the `Complete` button; the mark-all control
completes everything at once.

(specs: [G005](../../../../docs/gui_features.md#g005--launch-and-window-observation),
[G015](../../../../docs/gui_features.md#g015--button-by-accessible-name-with-enabled-state),
[G020 press](../../../../docs/gui_features.md#g020--press-action-on-a-control),
[G035 type](../../../../docs/gui_features.md#g035--type-text-into-a-field-and-enter-it),
[G080 list structure](../../../../docs/gui_features.md#g080--list-structure-with-content-and-emptiness-checks))

## Scenario: A completed todo moves to the Done list

- Given there is no file `todos.state`

- When I run `./todomvc`
- And I enter `Buy milk` into the `New task` textbox [G035]
- And I select the `Buy milk` listitem [G020]
- When I press the `Complete` button

- Then the `To do` list is empty [G080]
- And the `Done` list contains `Buy milk`

- When I press the `Clear completed` button

- Then the `To do` list is empty
- And the `Done` list is empty

## Scenario: Completing one task, then the remaining ones

- Given there is no file `todos.state`
- When I run `./todomvc`
- And I enter `Buy milk` into the `New task` textbox
- And I enter `Water the plants` into the `New task` textbox
- And I enter `Book the dentist` into the `New task` textbox

- Then the `Complete` button is disabled [G015]
  (because there is nothing selected)

- When I select the `Buy milk` listitem
- Then the `Complete` button is enabled

- When I press the `Complete` button
- And  I select the `Buy milk` listitem
- Then the `Complete` button is enabled [G015]
  (the completion toggle applies to done todos too: pressing it again
   would uncomplete the selected todo, cf. the widget naming contract)
- And  the `Done` list contains `Buy milk`
- And  the `To do` list does not contain `Buy milk`
- And  the `To do` list has `2` items
- And  the `Done` list has `1` item

- When I press the `Mark all as complete` button

- Then the `To do` list is empty
- And the `Done` list contains `Water the plants`
- And the `Done` list contains `Book the dentist`

The story is done with the application: its window is closed
explicitly (G095).

- When I close the window
