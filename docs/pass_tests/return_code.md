## Feature : return code test

### Scenario : NOK return code

Test an app returning non OK status  
  - When I run `uut -qsdqsd`
  - Then I get an error

### Scenario : OK return code

Test the return code of a normal run

  - When I run `uut -v`
  - Then I get no error