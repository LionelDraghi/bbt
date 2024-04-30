## Feature : “as in” test

Test the ability to share post-conditions between tests

### Scenario : Short option form
  - When I run `uut -v`  
  - Then I should get no error
  - And I get `version 1.0` 

### Scenario : Long form

  - When I run `uut --version`
  - Then I get just as in `Short option form` scenario