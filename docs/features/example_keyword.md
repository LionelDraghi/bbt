## Feature : The keyword `Example` is a synonym of the keyword `Scenario`.

Both should run the same way.

### Scenario : NOK return code

Test an app returning non OK status  
  - When I run `uut -qsdqsd`
  - Then I get an error

### Example : NOK return code

Test an app returning non OK status  
  - When I run `uut -qsdqsd`
  - Then I get an error
