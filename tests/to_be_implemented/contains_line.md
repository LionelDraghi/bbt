## Feature : testing that a file contains a line

bbt may be used to check that the output or a file contains a single line, thanks to the `contains` keyword.

### Scenario: test on a string output

  - When I run `uut -v`
  - Then the output should contains `version`

### Scenario : test on the error output

Fixme: not yet implemented.

  - When I run `uut -qsd`
  - Then the error output contains `unknown option -qsd`

### Scenario: test on a multiline output

  - When I run `uut -h`
  - Then the output should contains `--version`

### Scenario: test on a file

  - Given the file `config.ini` 
  ```
  mode=silent config.ini
  recurse=false config.ini
  ```
  - Then `config.ini` contains `mode=silent`

