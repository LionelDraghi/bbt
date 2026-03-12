<!-- omit from toc -->
## Feature:

bbt only recognize `-` as a step marker.  
Meaning that I can use other Markdown list marker `*` or `+` in the middle of the steps to build lists.  
I can even use `+ When`, it won't be recognized as a step.

_Table of Contents:_
- [Scenario: Different markers within the scenario](#scenario-different-markers-within-the-scenario)
- [Scenario: Step analysis is interrupted when exiting the section (#37)](#scenario-step-analysis-is-interrupted-when-exiting-the-section-37)

### Scenario: Different markers within the scenario

- Given the file `step_markers.md`
  ~~~md
  # Scenario:
  - Given the `config.ini` file
    ```
    ignore_hidden=true
    ```

  - Then `config.ini` contains `ignore_hidden=true`  
    This first step should succeed
  
  + Then `config.ini` contains `whatever`  
    (this comment line, if considered erroneously as a step, should cause the test to fail)
  + other list item
  
  * first list item
  * Then `config.ini` contains `whatever`  
    (this comment line, if considered erroneously as a step, should cause the test to fail)
  * other list item
  ~~~

- When I run `./bbt -c -q step_markers.md`
- Then I get no error
- And the output is
  ~~~
  ## Summary : **Success**, 1 scenarios OK
  ~~~

### Scenario: Step analysis is interrupted when exiting the section (#37)

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