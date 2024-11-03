## Feature : “when I run X or Y” test

Test the ability to duplicate a test thanks to "When I run X or Y" form

### Scenario : normal use case
  - When I run `uut -v` or `uut --version`
  - Then I should get no error
  - And the output should contains `version 1.0` 


### Scenario : the last command does not meet expectation (test should fail)
~~~
  - When I run `uut -v` or `uut --version` or `uut --qddfg` 
  - Then I should get no error
  - And the output should contains `unknown option --qddfg`

~~~