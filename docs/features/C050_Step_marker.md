## Feature:

bbt only recognized `-` as a step marker.
Meaning that I can use other Markdown list marker `*` or `+` in the middle of the steps to build lists.
I can even use `+ When`, it won't be recognized as a step.

### Scenario:
- Given the `config.ini` file
  ```
  ignore_hidden=true
  ```

- Then `config.ini` contains `ignore_hidden=true`  
  This first step should succeed

+ Then `config.ini` contains `whatever`  
  This comment line, if considered erroneously as a step, should cause the test to fail
+ other list item
  
* first list item
* Then `config.ini` contains `whatever`  
  This comment line, if considered erroneously as a step, should cause the test to fail
* other list item
