# Background feature

### Background :
  - Then there is no `config.ini` file

## Feature : “background” test

Background is a special scenario that is executed before every following tests.

### Scenario : normal use case
  - When I run `uut create config.ini` 
  - When I run `uut append "size=80x40" config.ini` 
  - Then `config.ini` is
  ```
  "size=80x40"
  ```

### Scenario : the last command does not meet expectation (test should fail)
  - When I run `uut -v` 
  - Then I should get no error

  This test should fail even if both assertion are satisfy, because Background isn't.