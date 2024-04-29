## Feature : return code test

### Scenario : NOK return code

Test an app returning non OK status  
  - When I run `uut -qsd`
  - Then I get an error

### Scenario : OK return code

Test an app returning non OK status

  - When I run `uut -v`
  - Then I get no error