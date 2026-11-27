
# Document: [add_a_task.md](../../../../Apps/ToDoMVC/docs/features/add_a_task.md)  
   ### Scenario: [The creation line is ready for a new task](../../../../Apps/ToDoMVC/docs/features/add_a_task.md): 
   - OK : Given there is no file `todos.state`  
   - OK : Then a window appears  
   - OK : And there is a textbox named `New task` [G015]  
   - OK : And there is a button named `Create`  
   - OK : And there is a list named `To do` [G080]  
   - OK : And the `To do` list is empty  
   - OK : And there is a list named `Done`  
   - OK : And the `Done` list is empty  
   - [X] scenario   [The creation line is ready for a new task](../../../../Apps/ToDoMVC/docs/features/add_a_task.md) pass  

   ### Scenario: [The user adds a task](../../../../Apps/ToDoMVC/docs/features/add_a_task.md): 
   - OK : When I type `Buy milk` into the `New task` textbox [G035]  
   - OK : And I press the `Create` button [G020]  
   - OK : Then the `To do` list contains `Buy milk`  
   - OK : And the window contains `Buy milk` [G010]  
   - OK : And the window does not contain `Book the dentist`  
   - OK : And the `Done` list is empty  
   - [X] scenario   [The user adds a task](../../../../Apps/ToDoMVC/docs/features/add_a_task.md) pass  

   ### Scenario: [The user adds several tasks](../../../../Apps/ToDoMVC/docs/features/add_a_task.md): 
   - OK : When I enter `Water the plants` into the `New task` textbox  
   - OK : Then the `To do` list contains `Water the plants`  
   - OK : When I enter `Book the dentist` into the `New task` textbox  
   - OK : Then the `To do` list contains `Book the dentist`  
   - OK : And the `Done` list is empty  
   - OK : When I close the window  
   - [X] scenario   [The user adds several tasks](../../../../Apps/ToDoMVC/docs/features/add_a_task.md) pass  


# Document: [complete_a_task.md](../../../../Apps/ToDoMVC/docs/features/complete_a_task.md)  
   ### Scenario: [A completed todo moves to the Done list](../../../../Apps/ToDoMVC/docs/features/complete_a_task.md): 
   - OK : Given there is no file `todos.state`  
   - OK : And I enter `Buy milk` into the `New task` textbox [G035]  
   - OK : And I select the `Buy milk` listitem [G020]  
   - OK : When I press the `Complete` button  
   - OK : Then the `To do` list is empty [G080]  
   - OK : And the `Done` list contains `Buy milk`  
   - OK : When I press the `Clear completed` button  
   - OK : Then the `To do` list is empty  
   - OK : And the `Done` list is empty  
   - [X] scenario   [A completed todo moves to the Done list](../../../../Apps/ToDoMVC/docs/features/complete_a_task.md) pass  

   ### Scenario: [Completing one task, then the remaining ones](../../../../Apps/ToDoMVC/docs/features/complete_a_task.md): 
   - OK : Given there is no file `todos.state`  
   - OK : And I enter `Buy milk` into the `New task` textbox  
   - OK : And I enter `Water the plants` into the `New task` textbox  
   - OK : And I enter `Book the dentist` into the `New task` textbox  
   - OK : Then the `Complete` button is disabled [G015]  
   - OK : When I select the `Buy milk` listitem  
   - OK : Then the `Complete` button is enabled  
   - OK : When I press the `Complete` button  
   - OK : And  I press the `Buy milk` listitem  
   - OK : Then the `Complete` button is enabled [G015]  
   - OK : And  the `Done` list contains `Buy milk`  
   - OK : And  the `To do` list does not contain `Buy milk`  
   - OK : And  the `To do` list has `2` items  
   - OK : And  the `Done` list has `1` item  
   - OK : When I press the `Mark all as complete` button  
   - OK : Then the `To do` list is empty  
   - OK : And the `Done` list contains `Water the plants`  
   - OK : And the `Done` list contains `Book the dentist`  
   - OK : When I close the window  
   - [X] scenario   [Completing one task, then the remaining ones](../../../../Apps/ToDoMVC/docs/features/complete_a_task.md) pass  


# Document: [edit_or_delete_a_task.md](../../../../Apps/ToDoMVC/docs/features/edit_or_delete_a_task.md)  
   ### Scenario: [The fresh app leaves the selection actions disabled](../../../../Apps/ToDoMVC/docs/features/edit_or_delete_a_task.md): 
   - OK : Given there is no file `todos.state`  
   - OK : Then a window named `ToDoMVC` appears  
   - OK : And there is a window named `ToDoMVC`  
   - OK : And the `Edit` button is disabled [G015]  
   - OK : And the `Delete` button is disabled  
   - [X] scenario   [The fresh app leaves the selection actions disabled](../../../../Apps/ToDoMVC/docs/features/edit_or_delete_a_task.md) pass  

   ### Scenario: [The user edits the second item, and that one is edited](../../../../Apps/ToDoMVC/docs/features/edit_or_delete_a_task.md): 
   - OK : Given there is no file `todos.state`  
   - OK : And I enter `Buy milk` into the `New task` textbox  
   - OK : And I enter `Water the plants` into the `New task` textbox  
   - OK : And I enter `Book the dentist` into the `New task` textbox  
   - OK : Then the `Edit` button is disabled  
   - OK : When I select the `Water the plants` listitem [G020]  
   - OK : Then the `Edit` button is enabled  
   - OK : When I press the `Edit` button  
   - OK : Then there is a textbox named `Edit todo`  
   - OK : When I type `Water the roses` into the `Edit todo` textbox [G035]  
   - OK : And I select the `Buy milk` listitem  
   - OK : Then the `To do` list contains `Water the roses` [G080]  
   - OK : And the `To do` list does not contain `Water the plants`  
   - OK : When I close the `ToDoMVC` window  
   - [X] scenario   [The user edits the second item, and that one is edited](../../../../Apps/ToDoMVC/docs/features/edit_or_delete_a_task.md) pass  


## Summary : **Success**, 7 scenarios OK

| Status     | Count |
|------------|-------|
| Failed     | 0     |
| Successful | 7     |
| Empty      | 0     |
| Not Run    | 0     |

