<!-- omit from toc -->
## Feature : The keyword `Example` is a synonym of the keyword `Scenario`.

To comply with Gherkin habits, both `Example` and `Scenario` are interchangeable.
(see https://cucumber.io/docs/gherkin/reference#example)

> [!Note] 
> `Examples` (plural) and `Scenarios` are a different concept in Gherkin

_Table of Contents:_
- [Scenario : NOK return code](#scenario--nok-return-code)
- [Example : NOK return code](#example--nok-return-code)

### Scenario : NOK return code

Test an app returning non OK status  
  - When I run `./sut -qsdqsd`
  - Then I get an error

### Example : NOK return code

Test an app returning non OK status  
  - When I run `./sut -qsdqsd`
  - Then I get an error
