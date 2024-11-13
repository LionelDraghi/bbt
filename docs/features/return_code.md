## Feature : return code test

Test the error code returned by an exe.  
Most OSes (but not all) implement such a mechanism.

### Scenario : NOK return code

Test an app returning non OK status  
  - When I run `sut -qsdqsd`
  - Then I get an error

### Example : OK return code

Test the return code of a normal run

  - When I run `sut -v`
  - Then I get no error