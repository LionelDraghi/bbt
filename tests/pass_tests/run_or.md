## Feature : “when I run X or Y” test

Test the ability to share pre and post-conditions between tests

### Scenario : normal use cas
  - When I run `uut -v` or `uut --version`
  - Then I should get no error
  - And I get `version 1.0` 

### Scenario : the last command does not meet expectation (test should fail)
  - When I run `uut -v` or `uut --version` or `uut --qddfg`
  - Then I should get no error
  - And I get `version 1.0` 
