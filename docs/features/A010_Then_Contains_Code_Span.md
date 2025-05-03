<!-- omit from toc -->
## Feature : testing that a file contains a string

bbt may be used to check that the output or a file contains a string, thanks to the `contains` or `contain` keyword.

_Table of Contents:_
- [Scenario: test on a string output](#scenario-test-on-a-string-output)
- [Scenario: test on a multiline output](#scenario-test-on-a-multiline-output)
- [Scenario: test on a file](#scenario-test-on-a-file)

### Scenario: test on a string output

  - When I run `./sut -v`
  - Then the output should contain `version`

### Scenario: test on a multiline output

  - When I run `./sut -h`
  - Then the output should contains `--version`

### Scenario: test on a file

  - Given the file `config.ini` 
  ```
  mode=silent config.ini
  recurse=false config.ini
  ```
  - Then `config.ini` contains `mode=silent`

