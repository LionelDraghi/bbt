## Scenario: Step analysis is interrupted when exiting the section ([Issue #37](https://github.com/LionelDraghi/bbt/issues/37))

- Given there is no `config.ini` file
  
- Given the file `step_markers2.md`
  ~~~md
  # Scenario 1


  # Other header

  - Then `config.ini` contains `ignore_hidden=true`  
    This line should be ignored : it's a step, but not in a scenario.
    (If not ignored, there will be an error as there is no config.ini file)

  ~~~

- When I successfully run `./bbt -c step_markers2.md`
- Then the output contains `- [ ] scenario [1](step_markers2.md) is empty, nothing tested`