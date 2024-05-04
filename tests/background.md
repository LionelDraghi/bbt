## Feature : “background” test

Background is a special scenario that is executed before every following tests.

### Background :
  - Given there is no `config.ini` file

### Scenario : normal use cas
  - When I run `uut create config.ini` 
  - When I run `uut append "size=80x40" config.ini` 
  - Then `config.ini` should be
  ```
  "size=80x40"
  ```

### Scenario : the last command does not meet expectation (test should fail)
  - When I run `uut -v` 
  - Then I should get no error

  This test should fail even if both assertion are satisfy, because Background isn't.