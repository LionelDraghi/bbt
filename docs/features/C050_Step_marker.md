<!-- omit from toc -->
## Feature:

bbt only recognize `-` as a step marker.  
Meaning that I can use other Markdown list marker `*` or `+` in the middle of the steps to build lists.  
I can even use `+ When`, it won't be recognized as a step.

_Table of Contents:_
- [Scenario:](#scenario)

### Scenario:
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
