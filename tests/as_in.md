## Feature : “as in” test

Test the –version option

### Scenario : Short option form
  - When I run `uut -v`
  - Then I get no error
  - And I get `uut v0.1.0` 

### Scenario : Long form

  - When I run `uut --version`
  - Then I get just as in `Short option form` scenario