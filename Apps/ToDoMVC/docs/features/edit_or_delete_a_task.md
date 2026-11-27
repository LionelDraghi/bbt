# Story: The user edits or deletes a task

Per the [widget naming contract](../../ToDoMVC.md), `Edit` and
`Delete` apply to the unique selected todo, and are insensitive while
the selection is empty. Selecting a todo is activating its title (the
`select` step on the item, G020); editing it opens the `Edit todo`
textbox, and the edition commits when another title is activated (the
blur of a real user).

(specs: [G015](../../../../docs/gui_features.md#g015--button-by-accessible-name-with-enabled-state),
[G020 press](../../../../docs/gui_features.md#g020--press-action-on-a-control),
[G035 type](../../../../docs/gui_features.md#g035--type-text-into-a-field-and-enter-it),
[G080 list structure](../../../../docs/gui_features.md#g080--list-structure-with-content-and-emptiness-checks))

## Scenario: The fresh app leaves the selection actions disabled

- Given there is no file `todos.state`
- When I run `./todomvc` [G005]
- Then a window named `ToDoMVC` appears
- And there is a window named `ToDoMVC`
(just a test of the alternative syntax)
- And the `Edit` button is disabled [G015]
- And the `Delete` button is disabled

## Scenario: The user edits the second item, and that one is edited

- Given there is no file `todos.state`
- When I run `./todomvc`
- And I enter `Buy milk` into the `New task` textbox
- And I enter `Water the plants` into the `New task` textbox
- And I enter `Book the dentist` into the `New task` textbox
- Then the `Edit` button is disabled
- When I select the `Water the plants` listitem [G020]
- Then the `Edit` button is enabled
- When I press the `Edit` button
- Then there is a textbox named `Edit todo`
- When I type `Water the roses` into the `Edit todo` textbox [G035]
- And I select the `Buy milk` listitem
- Then the `To do` list contains `Water the roses` [G080]
- And the `To do` list does not contain `Water the plants`

The story is done with the application: its window is closed
explicitly (G095).

- When I close the `ToDoMVC` window
