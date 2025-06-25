<!-- omit from toc -->
## Feature: Check that there is no output

Checking that there is no output is sometime useful.

_Table of Contents:_
- [Scenario: silent operation](#scenario-silent-operation)
- [Scenario: silent operation expected, but there is an output](#scenario-silent-operation-expected-but-there-is-an-output)

### Scenario: silent operation
- Given the new file `file.txt` containing `text1`

- When I run `./sut append text file.txt`

- Then there is no output
- And I get no error
  
### Scenario: silent operation expected, but there is an output

- Given the new `file.txt` file containing `text2`
- Given the new file `no_output.input`
  ```
  # Scenario:
  - When I run `./sut read file.txt`
  - Then there is no output
  ```

- When I run `./bbt -c no_output.input`

- Then I get an error
- And output contains `**NOK** : Then there is no output`
- And output contains `output not null`
