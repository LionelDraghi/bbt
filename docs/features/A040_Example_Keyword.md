## Feature : The keyword `Example` is a synonym of the keyword `Scenario`.

To comply with Gherkin habits, both `Example` and `Scenario` are interchangeable.

### Scenario : NOK return code

Test an app returning non OK status  
  - When I run `./sut -qsdqsd`
  - Then I get an error

### Example : NOK return code

Test an app returning non OK status  
  - When I run `./sut -qsdqsd`
  - Then I get an error
