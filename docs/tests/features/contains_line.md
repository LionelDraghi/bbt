# "contains" feature tests

## Feature : testing that a file contains a line

### Scenario: test on a string output

  - When I run `uut -v`
  - Then the output should contains `version`

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

 
